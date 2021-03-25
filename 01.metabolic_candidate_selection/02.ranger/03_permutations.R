######################################################
# Create a list of permuted dataframes on the y column
######################################################

# Seeds for permutations
my_seeds_for_permutations = map_dbl(.x = seq_along(1:n_permutations),
                                    .f = function(x){my_initial_seed + x})

# Create a list of permuted dfs (N = n_permutations)
permuted_dfs <- vector(mode = "list", length = n_permutations)

for (j in seq_along(1:n_permutations)){
  perm_df <- permute_dataframe_on_y_column(df = df, 
                                y_col = "phenotype", 
                                myseed = my_seeds_for_permutations[j])
  permuted_dfs[[j]] <- perm_df
}

# sprintf ensures that three digits are used in the column names
names(permuted_dfs) <- paste0("perm",sprintf('%0.3d', 1:n_permutations))

#####################################################################
# On each permuted df, create a train/test dataset and run a RF model
#####################################################################


# pre-allocate empty lists for results
j_models_accuracy <- vector("numeric", length = n_permutations)
j_variable_importances <- vector("list", length = n_permutations)

# Run n_permutations RF analyses and collect results
for (j in seq_along(1:n_permutations)){
  # get the jth permuted df
  permuted_df <- permuted_dfs[[j]]
  
  # create train and test set from the jth permuted dataframe 
  train_test_set <- create_train_test_sets(
                        data = permuted_df, 
                        myseed = my_seeds_for_permutations[j],
                        ratio = my_train_test_ratio)
  train_set <- train_test_set$train_set
  test_set <- train_test_set$test_set
  
  # fit RF model
  ranger_fit <- ranger(formula = phenotype ~ ., 
                       data = train_set,
                       num.trees = n_trees,
                       importance = "permutation",
                       min.node.size = 2,
                       class.weights = c(1/9, # resistant
                                         1/12), # sensitive
                       seed = my_seeds_for_permutations[j]
                       )
  cat(paste0(j,"th permutation;"), "Out-of-bag error:",ranger_fit$prediction.error,"\n")
  
  # predict phenotype
  predicted_phenotype <- predict(ranger_fit, test_set)
  
  # count success = good sample classification
  number_of_success <- 
    data.frame(predicted = predicted_phenotype$predictions,
               truth = test_set$phenotype) %>% 
    mutate("success" = predicted == truth) %>% 
    with(., sum(success)) # count the number of TRUE
  
  # model accuracy = number of samples / number of good classifications
  # store result
  model_accuracy <- number_of_success / nrow(test_set) * 100
  j_models_accuracy[j] <- model_accuracy
  

  # variable importance
  perm_name <- paste0("perm",sprintf('%0.4d', j)) # returns perm001 for 1st permutation for instance
  var_importances <- as.data.frame(ranger_fit$variable.importance) 
  colnames(var_importances) <- perm_name
  j_variable_importances[[j]] <- var_importances
}

permuted_var_imp <- bind_cols(j_variable_importances)

# clean up the R environment
rm(var_importances, 
   j_variable_importances)

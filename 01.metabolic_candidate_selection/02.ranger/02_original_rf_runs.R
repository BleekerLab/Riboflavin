
my_seeds = map_dbl(.x = seq_along(1:k_folds), 
                   .f = function(x){my_initial_seed + x})

# Creates a list with train and test sets (n = k_folds)
train_test_sets <- map(.x = my_seeds, 
                       .f = function(x){create_train_test_sets(data = df, 
                                                               myseed = x, 
                                                               ratio = my_train_test_ratio)})

# pre-allocate empty lists for results
k_models_accuracy <- vector("list", k_folds)
k_variable_importances <- vector("list", k_folds)

for (i in seq_along(1:k_folds)){
  # get train set
  train_set <- train_test_sets[[i]]$train_set
  # fit model
  ranger_fit <- ranger(
    formula = phenotype ~ ., 
    data = train_set, 
    num.trees = 10000,
    importance = "permutation",
    min.node.size = min_node_size,
    class.weights = c(1/9, # resistant
                      1/12), # sensitive
    seed = my_seeds[i])
  
  cat(paste0(i,"th k-fold original run;"), "Out-of-bag error:",ranger_fit$prediction.error,"\n")
  
  # predict phenotype from test set
  test_set <- train_test_sets[[i]]$test_set
  predicted_phenotype <- predict(ranger_fit, test_set)
  
  # count success = good sample classification
  number_of_success <- 
    data.frame(predicted = predicted_phenotype$predictions,
               truth = test_set$phenotype) %>% 
    mutate("success" = predicted == truth) %>% 
    with(., sum(success)) # count the number of TRUE
  
  # model accuracy = number of samples / number of good classifications
  model_accuracy <- number_of_success / nrow(test_set) * 100
  
  # return variable importances
  kfold_name <- paste0("kfold", sprintf('%0.2d', i))
  var_importances <- as.data.frame(ranger_fit$variable.importance) 
  colnames(var_importances) <- kfold_name
  
  # return results
  k_models_accuracy[[i]] <- model_accuracy
  k_variable_importances[[i]] <- var_importances

}

#################################
# Combine results into dataframes
#################################
# 1) original model accuracies
original_model_accuracy <- data.frame(
  kfold = 1:k_folds,
  accuracy = unlist(k_models_accuracy)
  )

# 2) original variable importances 
original_var_importances <- bind_cols(k_variable_importances) %>% as.data.frame() 
# var_imp_sd <- as.vector(apply(original_var_importances, 1, sd))
# var_imp_mean <- as.vector(apply(original_var_importances, 1, mean))
# original_var_importances$sd_var_imp <- var_imp_sd
# original_var_importances$mean_var_imp <- var_imp_mean


######################
# clean up environment
######################

# remove temporary objects used for looping etc.
rm(k_variable_importances, 
   var_imp_sd, 
   var_imp_mean, 
   k_models_accuracy, 
   predicted_phenotype)

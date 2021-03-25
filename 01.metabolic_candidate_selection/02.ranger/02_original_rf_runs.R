
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
    min.node.size = 2,
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
  #names(k_variable_importances)[[i]] <- kfold_name
  
}

# combine results
k_models_accuracy_df <- data.frame(kfold = 1:k_folds, 
                                   accuracy = unlist(k_models_accuracy))

original_var_importance <- bind_cols(k_variable_importances) %>% as.data.frame() 
var_imp_sd <- as.vector(apply(original_var_importance, 1, sd))
var_imp_mean <- as.vector(apply(original_var_importance, 1, mean))
original_var_importance$sd_var_imp <- var_imp_sd
original_var_importance$mean_var_imp <- var_imp_mean
original_var_importance <- rownames_to_column(original_var_importance, "metabolite")

#######
# Plots
#######
ggplot(k_models_accuracy_df, 
       aes(x = kfold, y = accuracy, fill = as.factor(kfold))) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(type = "qual", palette = 3) +
  scale_x_continuous(breaks = seq(from = 1, to = k_folds, by = 1))

original_var_importance %>% 
  arrange(desc(mean_var_imp)) %>% 
  top_n(10, wt = mean_var_imp) %>% 
  ggplot(., aes(x = metabolite, y = mean_var_imp)) +
  geom_bar(stat = "identity") +
  geom_errorbar(width = 0.2, 
                aes(ymin = mean_var_imp - sd_var_imp, 
                    ymax = mean_var_imp + sd_var_imp)) +
  coord_flip() +
  labs(x = "Metabolite", y = "Feature importance (average -/+ SD)")

ggsave(filename = "01.metabolic_candidate_selection/02.ranger/metabolic_candidates.png")

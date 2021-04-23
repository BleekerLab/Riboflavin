calculate_pvalue_for_each_feature <- function(original = original_var_importances, 
                                              permuted = permuted_var_importances){
  # original: a dataframe with p features and k-folds var importances (CV process)
  # permuted: a dataframe with p features and N permutations
  # fdr_method: a FDR method from the p.adjust.methods
  
  # how many features?
  # sanity check to see that the number of features correspond
  n_features <- nrow(original)
  stopifnot(nrow(original) == nrow(permuted))
  
  # Loop over features and collect p-values (one per feature)
  pvalues <- vector(mode = "numeric", length = n_features)
  for (i in seq_along(1:n_features)){
    
    # Compute average of original var. importance for feature ith
    mean_original_var_importance <- apply(original[i,], MARGIN = 1, FUN = mean)
    
    # extract permuted var. importance values of feature ith
    permuted_values <- permuted[i,] %>% as.numeric() 
      
    # calculate p-value
    pvalues[i] <- calculate_pvalue(original_value = mean_original_var_importance, 
                     distribution_of_random_values = permuted_values)
    
  }
  pvalues_df = data.frame(metabolite = row.names(original), pvalue = pvalues)
  return(pvalues_df)
}
  
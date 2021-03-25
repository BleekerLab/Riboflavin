# Custom functions

calculate_pvalue <- function(original_value, 
                             distribution_of_random_values, 
                             ndigits = 3){
  # Given an value e.g. Random Forest mean model accuracy from k-fold CV 
  # Compares with a distribution of random values e.g. RF model accuracies from permuted dfs
  # Returns a p-value rounded up to ndigits
  #
  # Remark: a p-value minimal limit is computed from the length of the distrition.
  # For instance, if 100 permutations are computed, then the limit of the p-value is 1/100 = 0.01
  # If the computed p-value is equal to 0, it is replaced by the p-value limit e.g. "p-value < 0.01" 
  #
  # For RF accuracy, we count the number of times the original accuracy is higher than the random accuracies
  
  p_value_detection_limit <- round(x = 1/length(distribution_of_random_values),
                                   digits = ndigits)
  
  # How many times is the original value was lower than the random values
  # This is the NULL hypothesis: my value is equal or lower than a given random value
  # If unlikely, it means that my accuracy is often higher than the random accuracy -> low p-value 
  computed_pvalue <- sum(original_value < distribution_of_random_values)/length(distribution_of_random_values)

  # Replace by p-value threshold (as given by the number of permutations) if equal to 0
  # Otherwise round up
  computed_pvalue <- ifelse(test = computed_pvalue == 0, 
                            yes = paste0("p-value < ", p_value_detection_limit), 
                            no = round(computed_pvalue, digits = ndigits))
  
  return(computed_pvalue)
}
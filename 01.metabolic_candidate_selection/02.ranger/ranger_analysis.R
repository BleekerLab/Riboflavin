###########################################
# Load all required libraries and functions
# Makes use of Microsoft MRAN checkpoint
########################################
source("01.metabolic_candidate_selection/02.ranger/00_load_libraries.R")

# import custom functions
source("01.metabolic_candidate_selection/02.ranger/create_train_test_sets.R")
source("01.metabolic_candidate_selection/02.ranger/permute_dataframe.R")
source("01.metabolic_candidate_selection/02.ranger/custom_functions/compute_pvalue.R")

########
# PARAMS
########
rf_params <- read_yaml(file = "01.metabolic_candidate_selection/02.ranger/rf_params.yaml")

my_initial_seed = rf_params$my_initial_seed
k_folds = rf_params$k_folds
my_train_test_ratio <- rf_params$my_train_test_ratio
n_permutations <- rf_params$n_permutations

# RF ranger function specific parameters
n_trees <- rf_params$n_trees
min_node_size <- rf_params$min_node_size


####################################################################
# Import dataframe with both genotype info and metabolite abundances
# Outliers removed: 
#   - IL26
#   - s_ch_1
####################################################################

# produces an object called "df" that has X features and one y class column
source("01.metabolic_candidate_selection/02.ranger/01_import_data.R")


################################
# Original RF runs (k = k_folds)
################################

source("01.metabolic_candidate_selection/02.ranger/02_original_rf_runs.R")

# Returns
# 1) original_model_accuracy = dataframe with original model accuracy 
# 2) original_var_importances = dataframe with original variable importances +  mean/sd var imp

#######################################
# Permuted RF runs (j = n_permutations)
#######################################

# returns j_variable_importances
# returns j_models_accuracy
source("01.metabolic_candidate_selection/02.ranger/03_permutations.R")

# Returns
# 1) permuted_model_accuracies = a vector with one model accuracy per permutation
# 2) permuted_var_importances = dataframe with variable importances from permuted data

########################################################
# Create plot of model accuracy versus random accuracies
########################################################

mean_acc <- mean(original_model_accuracy$accuracy)
sd_acc <- sd(original_model_accuracy$accuracy)
upper_sd_acc <- ifelse(test = mean_acc + sd_acc > 1, yes = 100, mean_acc + sd_acc)


# Compute p-value to add to the model plot
model_pvalue <- calculate_pvalue(original_value = mean_acc, 
                                 distribution_of_random_values = permuted_model_accuracies)

plot_title <- paste0("Model accuracy versus random accuracies\n",
                     "(",n_permutations," permutations)","\n",
                     ifelse(test = is.character(model_pvalue), yes = model_pvalue, no = paste0("p-value: ",model_pvalue))
                     )

plot_model <- 
  ggplot(data = data.frame(accuracy = permuted_model_accuracies), aes(x = accuracy)) +
    geom_density(kernel = "gaussian") +
    # average of the original RF accuracies
    geom_vline(xintercept = mean_acc, color = "blue") +
    geom_vline(xintercept = (mean_acc - sd_acc), color = "red", linetype = "dashed") +
    geom_vline(xintercept = upper_sd_acc, color = "red", linetype = "dashed") +
  ggtitle(plot_title)

plot_model

ggsave(filename = "01.metabolic_candidate_selection/02.ranger/model_accuracy_vs_randoms.png", 
       plot = plot_model)  

#############################################
# Calculate p-value for each metabolite + FDR
#############################################

# bind the mean var importance (original) + distribution of permuted var imp
# for each feature/metabolite, extract a vector with original value + permuted values
# for each of these vector, calculate the p-value



###############################################################
# Save object (to avoid having to run the whole analysis again)
###############################################################

save(original_model_accuracy, 
     original_var_importances, 
     permuted_model_accuracies, 
     permuted_var_importances,
     file = "01.metabolic_candidate_selection/02.ranger/results_ranger_analysis.RData")
 


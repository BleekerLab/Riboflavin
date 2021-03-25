###########################################
# Load all required libraries and functions
# Makes use of Microsoft MRAN checkpoint
########################################
source("01.metabolic_candidate_selection/02.ranger/00_load_libraries.R")

# import custom functions
source("01.metabolic_candidate_selection/02.ranger/create_train_test_sets.R")
source("01.metabolic_candidate_selection/02.ranger/permute_dataframe.R")

########
# PARAMS
########
rf_params <- read_yaml(file = "01.metabolic_candidate_selection/02.ranger/rf_params.yaml")

my_initial_seed = rf_params$my_initial_seed
k_folds = rf_params$k_folds
my_train_test_ratio <- rf_params$my_train_test_ratio
n_permutations <- rf_params$n_permutations

# ranger function specific parameters
n_trees <- rf_params$n_trees


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

# returns a dataframe with variable importances: var_imp_df
# returns k_models_accuracy_df
source("01.metabolic_candidate_selection/02.ranger/02_original_rf_runs.R")

#######################################
# Permuted RF runs (j = n_permutations)
#######################################

# returns j_variable_importances
# returns j_models_accuracy
source("01.metabolic_candidate_selection/02.ranger/03_permutations.R")

########################################################
# Create plot of model accuracy versus random accuracies
########################################################

mean_acc <- mean(k_models_accuracy_df$accuracy)
sd_acc <- sd(k_models_accuracy_df$accuracy)

upper_sd_acc <- ifelse(test = mean_acc + sd_acc > 1, yes = 100, mean_acc + sd_acc)

ggplot(data = data.frame(accuracy = j_models_accuracy), aes(x = accuracy)) +
  geom_density() +
  # average of the original RF accuracies
  geom_vline(xintercept = mean_acc, color = "blue") +
  geom_vline(xintercept = (mean_acc - sd_acc), color = "red", linetype = "dashed") +
  geom_vline(xintercept = upper_sd_acc, color = "red", linetype = "dashed")
  
ggsave(filename = "01.metabolic_candidate_selection/02.ranger/model_accuracy_vs_randoms.png")  


###############################################################
# Save object (to avoid having to run the whole analysis again)
###############################################################

# save(k_models_accuracy_df, 
#      j_models_accuracy, 
#      var_imp_df, 
#      j_variable_importances, 
#      file = "01.metabolic_candidate_selection/02.ranger/results_ranger_analysis.RData")
# 


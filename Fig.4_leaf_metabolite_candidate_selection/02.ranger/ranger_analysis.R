###########################################
# Load all required libraries and functions
# Makes use of Microsoft MRAN checkpoint
########################################
source("01.metabolic_candidate_selection/02.ranger/00_load_libraries.R")

# import custom functions
source("01.metabolic_candidate_selection/02.ranger/custom_functions/create_train_test_sets.R")
source("01.metabolic_candidate_selection/02.ranger/custom_functions/permute_dataframe.R")
source("01.metabolic_candidate_selection/02.ranger/custom_functions/compute_pvalue.R")
source("01.metabolic_candidate_selection/02.ranger/custom_functions/compute_pvalue_for_each_feature.R")

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

df <- df %>% 
  # not required for ML classification
  select(- sample, - genotype) 


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

# a dataframe to accomodate summary values 
# Useful to pass to ggplot2
vlines_df = data.frame(
  mean_acc = mean(original_model_accuracy$accuracy),
  sd_acc = sd(original_model_accuracy$accuracy)
  )

vlines_df = vlines_df %>% 
  mutate(lower_sd_acc = ifelse(test = mean_acc - sd_acc == 0, yes = 0, mean_acc - sd_acc),
         upper_sd_acc = ifelse(test = mean_acc + sd_acc > 1, yes = 100, mean_acc + sd_acc)) %>%
  rownames_to_column("id") %>% 
  pivot_longer(cols = -id) %>% 
  select(-id) %>% 
  dplyr::filter(name != "sd_acc") %>% 
  mutate(linetype = c(1,"dotted","dotted"))

# Compute p-value to add to the model plot
model_pvalue <- calculate_pvalue(original_value = mean(original_model_accuracy$accuracy), 
                                 distribution_of_random_values = permuted_model_accuracies)

plot_title <- paste0("Model accuracy versus random accuracies\n",
                     "(",n_permutations," permutations)","\n",
                     ifelse(test = is.character(model_pvalue), yes = model_pvalue, no = paste0("p-value: ",model_pvalue))
                     )

plot_model <- 
  ggplot(data = data.frame(accuracy = permuted_model_accuracies), aes(x = accuracy)) +
    geom_density(kernel = "gaussian") +
    # average of the original RF accuracies
    geom_vline(data = vlines_df, aes(xintercept = value, colour = name, linetype = linetype), show.legend = TRUE) +
  ggtitle(plot_title) +
  scale_colour_discrete(labels = c("Lower model accuracy (Mean - SD)", 
                                   "Mean model accuracy",
                                   "Upper model accuracy (Mean + SD)")) +
  theme(legend.title = element_blank(),
      legend.position = "bottom")
plot_model

ggsave(filename = "01.metabolic_candidate_selection/02.ranger/plots/model_accuracy_vs_randoms.png", 
       plot = plot_model)  

#############################################
# Calculate p-value for each metabolite + FDR
#############################################

# bind the mean var importance (original) + distribution of permuted var imp
# for each feature/metabolite, extract a vector with original value + permuted values
# for each of these vector, calculate the p-value
uncorrected_pvals_df <- calculate_pvalue_for_each_feature(original = original_var_importances, 
                                                     permuted = permuted_var_importances)

selected_features <- 
  original_var_importances %>% 
  rownames_to_column("metabolite") %>% 
  rowwise() %>% 
  mutate(average_importance = mean(c_across(where(is.numeric))), 
         sd_importance = sd(c_across(where(is.numeric)))) %>% 
  select(metabolite, average_importance, sd_importance) %>% 
  inner_join(., y = uncorrected_pvals_df, by = "metabolite")

final_selected_features =
  selected_features %>% 
  # dynamic filtering based on permutation number 
  # e.g. 100 perm = 0.01 threshold
  filter(pvalue < 1/n_permutations) %>% 
  arrange(desc(average_importance), pvalue) %>% 
  mutate(pvalue = ifelse(test = pvalue == 0, 
                yes = paste0("p-value < ", 1/n_permutations), no = pvalue))


write.csv(final_selected_features, 
          file = "01.metabolic_candidate_selection/02.ranger/final_candidates.csv", 
          quote = F, 
          row.names = F)



###########################
# Create plot of candidates
###########################

source("01.metabolic_candidate_selection/02.ranger/01_import_data.R")

# Plot abundance per genotype
df4plot <- 
  df %>% 
  pivot_longer(cols = - c("phenotype", "genotype", "sample"), 
               names_to = "metabolite", 
               values_to = "abundance") %>% 
  filter(metabolite %in% final_selected_features$metabolite) %>% 
  mutate(metabolite = factor(metabolite, levels = final_selected_features$metabolite)) 

# plot with free y scaling
ggplot(df4plot, aes(x = genotype, y = abundance, fill = phenotype)) +
    geom_boxplot() +
    geom_point() +
    facet_wrap(~ metabolite, scales = "free") +
    theme(axis.text.x = element_text(angle = 90)) 

# PNG format
ggsave(filename = "01.metabolic_candidate_selection/02.ranger/plots/final_candidates.png", 
       width = 16, 
       height = 10)
# PDF format
ggsave(filename = "01.metabolic_candidate_selection/02.ranger/plots/final_candidates.pdf", 
       width = 16, 
       height = 10)

# Which metabolites are more abundant in R class?
metabolites_more_abundant_in_resistant_genotypes = 
  df4plot %>% 
  group_by(phenotype, metabolite) %>% 
  summarise(mean_abundance = mean(abundance)) %>% 
  pivot_wider(id_cols = metabolite, names_from = phenotype, values_from = mean_abundance) %>% 
  filter(resistant > sensitive) 

write.csv(metabolites_more_abundant_in_resistant_genotypes, 
          file = "01.metabolic_candidate_selection/02.ranger/candidates_more_abundant_in_resistant.csv", 
          quote = F, 
          row.names = F)

# plot with COMMON y scaling
ggplot(df4plot, aes(x = genotype, y = abundance, fill = phenotype)) +
  geom_boxplot() +
  geom_point() +
  facet_wrap(~ metabolite, scales = "fixed") +
  theme(axis.text.x = element_text(angle = 90)) 

# PNG format
ggsave(filename = "01.metabolic_candidate_selection/02.ranger/plots/final_candidates_common_y_scale.png", 
       width = 16, 
       height = 10)

###############################################################
# Save object (to avoid having to run the whole analysis again)
###############################################################

save(original_model_accuracy, 
     original_var_importances, 
     permuted_model_accuracies, 
     permuted_var_importances,
     file = "01.metabolic_candidate_selection/02.ranger/results_ranger_analysis.RData")
 


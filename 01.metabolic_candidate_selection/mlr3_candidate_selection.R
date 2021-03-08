library("checkpoint")
checkpoint("2020-12-01") # all packages in the same R project will be taken from this date
library("mlr3")
library("mlr3viz")
library("mlr3verse")
library("tidyverse")
library("precrec")

####################################################################
# Import dataframe with both genotype info and metabolite abundances
####################################################################

peaks <- read.csv("01.metabolic_candidate_selection/genotype_and_peak_data.csv")

sample_info <- read.csv("01.metabolic_candidate_selection/sample_genotype_phenotype.csv")

df <- bind_cols(sample_info, peaks) %>% 
  select(- sample, - genotype) # not required for ML classification

#########################
# Define Task and Learner
#########################
task_metabolites <- TaskClassif$new(id = "peaks", 
                                    backend = df, 
                                    target = "phenotype", 
                                    positive = "resistant")


rf_learner = lrn(.key = "classif.ranger", 
              id = "rf", 
              importance = "permutation", 
              num.trees = 10000)

filter_ranger = flt("importance", learner = rf_learner)
filter_ranger$calculate(task_metabolites)
feature_importances = as.data.table(filter_ranger) %>% 
  arrange(desc(score)) %>% 
  head(n = 10)
ggplot(feature_importances, aes(x = feature, y = score)) + 
  geom_histogram(stat = "identity") +
  coord_flip()

############################################################
# Train model and estimate accuracy of the model on test set
############################################################
# train_set = sample(x = task_metabolites$nrow, size = 0.7 * task_metabolites$nrow)
# test_set = sample(task_metabolites$nrow, size = 0.3 * task_metabolites$nrow)
# 
# learner$train(task_metabolites, row_ids = train_set)
# print(learner$model)
# 
# prediction = learner$predict(task_metabolites, row_ids = test_set)
# prediction$confusion
# autoplot(prediction)
# prediction$score(measures = msr("classif.acc"))

##############################################################################################
# Cross-validation (to improve generalisation capacity and test stability of different splits)
##############################################################################################

cv_strategy = rsmp(.key = "cv", folds = 10)
cv_strategy$instantiate(task_metabolites)

results_from_cv_models = resample(task = task_metabolites, 
                     learner = rf_learner, 
                     resampling = cv_strategy, 
                     store_models = TRUE)


results_from_cv_models$aggregate(msr("classif.acc"))   # accuracy
results_from_cv_models$aggregate(msr("classif.bacc"))  # balanced accuracy

################
# Compare models
################


learners = lrns(c("classif.rpart", "classif.ranger"), predict_type = "response")

bm_design = benchmark_grid(
  tasks = task_metabolites,
  learners = learners,
  resamplings = rsmp("cv", folds = 10)
)
bmr = benchmark(bm_design)

measures = msrs(c("classif.ce", "classif.acc"))
performances = bmr$aggregate(measures)



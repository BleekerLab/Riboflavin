library("checkpoint")
checkpoint("2020-12-01") # all packages in the same R project will be taken from this date
library("mlr3")
library("mlr3viz")
library("mlr3verse")

####################################################################
# Import dataframe with both genotype info and metabolite abundances
####################################################################

df <- read.csv("01.metabolic_candidate_selection/genotype_and_peak_pos_selection_nontrans.csv")


#########################
# Define Task and Learner
#########################
task_metabolites <- TaskClassif$new(id = "genotype", backend = df, target = "Genotype")
learner = mlr_learners$get("classif.ranger")

train_set = sample(x = task_metabolites$nrow, size = 0.7 * task_metabolites$nrow)
test_set = sample(task_metabolites, size = 0.3 * task_metabolites$nrow)

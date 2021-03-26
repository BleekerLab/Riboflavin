# Random Forest metabolite selection

Tools:
* ranger

## Random Forest model accuracy
The Random Forest model has an accuracy of 90% -/+ 16.1. (74% - 100%). 

Comparison to a distribution of random RF model accuracies.

<img src="./plots/model_accuracy_vs_randoms.png" width="600px" alt="RF model accuracy">

## Significant candidates (own scaling)

Classified by highest variable importance to lowest. 
All candidates significant (e.g. p < 0.01 if 100 permutations).

<img src="./plots/final_candidates.png" width="100%" alt="Metabolic candidates">


## Significant candidates (own scaling)

<img src="./plots/final_candidates_common_y_scale.png" width="100%" alt="Metabolic candidates">

## Table of candidates

[Final candidates are in this .csv table](./final_candidates.csv)
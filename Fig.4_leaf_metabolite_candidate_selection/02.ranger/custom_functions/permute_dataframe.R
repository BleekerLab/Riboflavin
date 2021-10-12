suppressPackageStartupMessages(library("gtools")) # permute() function
suppressPackageStartupMessages(library("dplyr"))
suppressPackageStartupMessages(library("rlang"))

# Permute a dataframe on the class (y) column
# This is useful to compute a distribution of Random Forest model accuracies based on randomly permuted dfs
permute_dataframe_on_y_column <- function(df, y_col = "phenotype", myseed = 123){
  # enquo defuses expressions supplied as function arguments
  # https://rlang.r-lib.org/reference/quotation.html
  y_column <- rlang::enquo(y_col)
  
  # extract original y vector of classes
  y <- df[,y_col] 
  
  # randomly permute based on the choosen seed
  set.seed(myseed)
  permuted_y <- permute(y)
  
  # replace original column with permuted values
  permuted_df <- df %>% 
    dplyr::select(-!!y_column) %>% # remove original column
    dplyr::mutate(!!y_column := permuted_y)
    
  return(permuted_df)
}
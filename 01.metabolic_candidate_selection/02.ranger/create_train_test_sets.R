create_train_test_sets <- function(data, myseed = 123, ratio = 0.8){
  set.seed(myseed)
  
  sampled_rows_train <- sample(x = seq(1:nrow(data)), 
                               size = round(ratio*nrow(data),digits = 0)
  )
  train_set <- data[sampled_rows_train, ]
  test_set <- data[-sampled_rows_train, ]
  
  return(list("train_set" = train_set, 
              "test_set" = test_set))
}
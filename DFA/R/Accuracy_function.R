#' @title A function to test the accuracy of LDFA groups assignments.
#' @description This function takes your predicted group assignments of the training and test data and compares it to the actual group assignments. It then takes the mean to show the porpotion correct for each dataset.
#' @param train_pred predicted assignments of training data
#' @param test_pred predicted assignments of test data
#' @param partitioned_list previously partitioned data using the partition function
#' @keywords Accuracy
#' @export


#### Tests for Accuracy

Accuracy <- function(train_pred, test_pred, partitioned_list) {
  test <- mean(test_pred$class==x$test$Species)
  train <- mean(train_pred$class==x$train$Species)
  return(list(train=train,test=test))
}

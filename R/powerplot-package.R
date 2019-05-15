#' @title A function to predict groups assignments.
#' @description This function takes a specified model of your groups ~ predictor variables and then uses that model to predict group membership of you specified data.
#' @param model groups ~ predictor variables
#' @param data data used to predict group assignments
#' @keywords Prediction
#' @export
DFA_pred <- function(model, data) {
  LDA_object <- MASS::lda(model, data)
  Predict.lda.values <- predict(LDA_object, data)
  return(Predict.lda.values)
}
#' @title A function to partition data sets for use in LDFA.
#' @description These functions allow you to partition the observations in your data set in to test and training subsets.
#' @param x your data
#' @param p fraction of observations allocated to training set
#' @keywords partition
#' @export
Partition <- function(x, p=.8) {
  sample <- sample.int(n = nrow(x), size = floor(p*nrow(x)), replace = F)
  train <- x[sample, ]
  test  <- x[-sample, ]
  return(list(train=train,test=test))
}
#' @title A function to plot predicted groups assignments.
#' @description This function takes prediction data and plots it using ggplot.
#' @param data predicted group assignments derived from the DFA_pred function.
#' @keywords Prediction plot
#' @export
DFA_plot <- function(data) {
  v <- as.data.frame(data)
  g<- ggplot2::ggplot(v, ggplot2::aes(v$x.LD1, v$x.LD2)) +
    ggplot2::geom_point(ggplot2::aes(color = v$class))
  g <- g + ggplot2::ggtitle("LDA Predictions")
  g <- g + ggplot2::xlab("LD1")
  g <- g + ggplot2::ylab("LD2")
}
#' @title A function to test the accuracy of LDFA groups assignments.
#' @description This function takes your predicted group assignments of the training and test data and compares it to the actual group assignments. It then takes the mean to show the porpotion correct for each dataset.
#' @param train_pred predicted assignments of training data
#' @param test_pred predicted assignments of test data
#' @param x previously partitioned data using the partition function
#' @keywords Accuracy
#' @export
Accuracy <- function(train_pred, test_pred, x) {
  test <- mean(test_pred$class==x$test$class)
  train <- mean(train_pred$class==x$train$class)
  return(list(train=train,test=test))
}


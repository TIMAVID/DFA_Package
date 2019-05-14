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



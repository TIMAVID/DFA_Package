#' @title A function to partition data sets for use in LDFA.
#' @description These functions allow you to partition the observations in your data set in to test and training subsets.
#' @param x your data
#' @param p fraction of observations allocated to training set
#' @keywords partition
#' @export
#' @examples
Partition (x, p=.8)

#### Split the data into training (80%) and test (20%) sets

Partition <- function(x, p=.8) {
  sample <- sample.int(n = nrow(x), size = floor(p*nrow(x)), replace = F)
  train <- x[sample, ]
  test  <- x[-sample, ]
  return(list(train=train,test=test))
}

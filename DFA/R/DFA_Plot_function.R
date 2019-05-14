#' @title A function to plot predicted groups assignments.
#' @description This function takes prediction data and plots it using ggplot.
#' @param data predicted group assignments derived from the DFA_pred function.
#' @keywords Prediction plot
#' @export


#### Plotting the result in ggplot

DFA_plot <- function(data) {
  v <- as.data.frame(data)
  g<- ggplot2::ggplot(v, aes(v$x.LD1, v$x.LD2)) +
    geom_point(ggplot2::aes(color = v$class))
  g <- g + ggplot2::ggtitle("LDA Predictions")
  g <- g + ggplot2::xlab("LD1")
  g <- g + ggplot2::ylab("LD2")
}

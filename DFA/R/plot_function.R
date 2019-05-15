#' @title A function to plot the result of your LDFA using ggplot2.
#' @description This function allows you to plot the results of your LDFA using ggplot2.
#' @param prediction The result of performing DFA_pred on your training or test data.
#' @keywords plot
#' @export
#' @examples
DFA_plot(DFA_pred_results)

DFA_plot <- function(prediction) {
  v <- as.data.frame(data)
  g<- ggplot2::ggplot(v, aes(v$x.LD1, v$x.LD2)) +
    geom_point(aes(color = v$class))
  g <- g + ggplot2::ggtitle("LDA Predictions")
  g <- g + ggplot2::xlab("LD1")
  g <- g + ggplot2::ylab("LD2")
}


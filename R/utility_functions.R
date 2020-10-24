#' Plot the tracer breakthrough curve
#'
#' @param tracer_data tibble or data.frame. Tracer breakthrough data.
#' The fist column should contain the time and the second the tracer concentration.
#' @param time_threshold numeric. Time of the breakthrough (i.e. start of the concentration increase)
#' @param file_name string. The name for the pdf file to save the figure. Default is NULL
#' @param save_pdf logical. Default is FALSE
#'
#' @return ggplot object
#' @export
#'
#' @examples
#' plot_tracer(tracer_data = tracer, time_threshold = 40000)
plot_tracer <- function(tracer_data, time_threshold = NULL, file_name = NULL,
                       save_pdf = FALSE) {

  g <- ggplot2::ggplot(data = tracer_data,
                  aes_string(x = colnames(tracer_data)[1],
                             y = colnames(tracer_data)[2])) +
    geom_line() +
    geom_point()
  if(!is.null(time_threshold)) {
    g <- g + geom_vline(xintercept = time_threshold, colour = 'blue', lty = 2)
  }
  else g
  if(save_pdf) ggsave(file_name)
  g
}

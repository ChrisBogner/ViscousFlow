#' Plot the tracer breakthrough curve
#'
#' @param tracer_data tibble or data.frame. Tracer breakthrough data. The first column must be time, the second the tracer concentration.
#' @param time_threshold numeric. Time of the breakthrough (i.e. start of the concentration increase)
#' @param xlab_tag string or expression for the x label. Default is Time (sec).
#' @param ylab_tag string or expression for the y label. Default is c/c0.
#' @param file_name string. The name for the pdf file to save the figure. Default is NULL.
#' @param save_pdf logical. Should de figure be saved to a pdf file. Default is FALSE.
#'
#' @return ggplot object
#' @export
#'
#' @examples
#' data(tracer)
#' plot_tracer(tracer_data = tracer, time_threshold = 40000)
plot_tracer <- function(tracer_data, time_threshold = NULL,
                        xlab_tag = 'Time (sec)', ylab_tag = expression(c/c[0]),
                        file_name = NULL,
                        save_pdf = FALSE) {

  g <- ggplot2::ggplot(data = tracer_data,
                       ggplot2::aes_string(x = colnames(tracer_data)[1],
                             y = colnames(tracer_data)[2])) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::xlab(xlab_tag) +
    ggplot2::ylab(ylab_tag)
  if(!is.null(time_threshold)) {
    g <- g + ggplot2::geom_vline(xintercept = time_threshold, colour = 'blue', lty = 2)
  }
  else g
  if(save_pdf) ggplot2::ggsave(file_name)
  g
}

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

extract_data <- function(data_orig, weight_unit) {
  time_xs <- data_orig %>%
    dplyr::slice(seq(1, dim(data_orig)[1], 2)) %>%
    dplyr::pull(var = 1) %>%
    stringr::str_sub(start = 1, end = 8) %>%
    lubridate::hms()

  time_rel <- lubridate::time_length(time_xs - time_xs[1], unit = 'seconds')

  dat <- data_orig %>%
    dplyr::slice(seq(2, dim(data_orig)[1], 2)) %>%
    dplyr::pull(var = 1) %>%
    stringr::str_remove_all('\\s') %>%
    stringr::str_sub(start = 1, end = -(stringr::str_length(weight_unit)+1)) %>%
    as.numeric()
  res <- tibble::tibble('time_rel_sec' = time_rel, dat)
  colnames(res)[2] <- paste0('weight_', weight_unit)

  g <- ggplot2::ggplot(res, ggplot2::aes_string(x = 'time_rel_sec',
                                                y = colnames(res)[2])) +
    ggplot2::geom_line()
  print(g)
  res
}


calculate_rel_weights <- function(scale_s, rho_H20 = 0.99704, D, delta_out = 30, pump_rate = 2.0) {

  pump_rate <- pump_rate * 60 /(rho_H20 * 1000 * (D/2)^2 * pi)

  delta_t <- scale_s %>%
    dplyr::pull(var = 1) %>%
    diff() %>%
    mean() %>%
    round()

  # sampling_time <- scale_s %>%
  #   dplyr::slice(seq(1, dim(scale_s)[1], by = delta_out/delta_t)) %>%
  #   dplyr::pull(var = 1)

  res <- scale_s %>%
    dplyr::pull(var = 2) %>%
    diff()

  res_clean <- ifelse(abs(res) >= stats::quantile(res, probs = 0.99), 0, res)
  res_clean <- ifelse(res_clean < 0, 0, res_clean)
  res_clean <- c(0,cumsum(res_clean))

  res_direct <- res_clean[seq(1, dim(scale_s)[1], by = delta_out/delta_t)] %>%
    diff()

  flow_rate <- c(0, res_direct / delta_out)
  flow_rate <- flow_rate * 3600 /(rho_H20 * 1000 * (D/2)^2 * pi)

  flow_rate_smoothed <- sfsmisc::D1ss(unlist(scale_s[,1]), res_clean) * 3600 /(rho_H20 * 1000 * (D/2)^2 * pi)
  flow_rate_smoothed <- ifelse(flow_rate_smoothed < 0, 0, flow_rate_smoothed)

  #flow_rate_smooth <- loess(flow_rate ~ sampling_time,
  #span = 0.2)
  #res_smooth <- predict(flow_rate_smooth, sampling_time)
  #flow_rate <- ifelse(flow_rate > 0.7*pump_rate, res_smooth, flow_rate)

  tibble::tibble(sec = unlist(scale_s[,1]), flow_rate_smoothed)

}

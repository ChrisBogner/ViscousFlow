#' Calculate the breakthrough time of a tracer
#'
#' The breakthrough time of a tracer in the drainage of a soil column is calculated as the maximum of curvature of its concentration in a suitable time interval.
#'
#'
#' @param tracer_data tibble or data.frame. Tracer breakthrough data. The first column must be time, the second the tracer concentration.
#' @param time_interval numeric vector of length 2. The time interval to search for the breakthrough, typically early in the experiment.
#' @param smooth logical. Should the tracer data be smoothed by loess first. Default is true.
#' @param loess_span numeric. The smoothing parameter for the loess smoother. Default is 0.2.
#' @param xlab_tag string or expression for the x label. Default is Time (sec).
#' @param ylab_tag string or expression for the y label. Default is c/c0.
#' @param do_plot logical. Should a plot be produced. Default is TRUE.
#'
#' @return list.
#' breakthrough_time The estimated time of the tracer breakthrough.
#' tracer_data The original tracer data augmented by the smoothed data (is smooth = TRUE), first derivative of the tracer data (slope),
#' calculated curvature and its first derivative
#' @export
#'
#' @examples
#' data(tracer)
#' find_tracer_breakthrough(tracer_data = tracer, time_interval = c(30000, 40000))
find_tracer_breakthrough <- function(tracer_data, time_interval, smooth = TRUE,
                                     loess_span = 0.2,
                                     xlab_tag = 'Time (sec)',
                                     ylab_tag = expression(c/c[0]),
                                     do_plot = TRUE) {

  if(smooth) {
    tracer_smoothed <-stats::loess(unlist(tracer_data[,2]) ~ unlist(tracer_data[,1]),
                                   span = loess_span)

    res <- stats::predict(tracer_smoothed, unlist(tracer_data[,1]))
    tracer_data <- tracer_data %>%
      dplyr::mutate(smoothed_tracer = res)

    d1 <- sfsmisc::D1ss(unlist(tracer_data[,1]), unlist(tracer_data$smoothed_tracer))
    d2 <- sfsmisc::D2ss(unlist(tracer_data[,1]), unlist(tracer_data$smoothed_tracer))$y
  } else {
    d1 <- sfsmisc::D1ss(unlist(tracer_data[,1]), unlist(tracer_data[,2]))
    d2 <- sfsmisc::D2ss(unlist(tracer_data[,1]), unlist(tracer_data[,2]))$y
  }

  # formula for curvature from Wikipedia (https://en.wikipedia.org/wiki/Curvature)
  curvature <- abs(d2)/((1+d1^2)^(3/2))
  # Change of curvature
  d1_curve <- sfsmisc::D1ss(unlist(tracer_data[,1]), curvature)
  ind_search <- which(unlist(tracer_data[,1]) > time_interval[1] & unlist(tracer_data[,1]) < time_interval[2])
  # Pick the max of curvature as tracer breakthrough
  breakthrough_time <- unlist(tracer_data[(which.max(curvature[ind_search]) + ind_search[1] - 1), 1])

  tracer_data <- tracer_data %>%
    dplyr::mutate(tracer_first_deriv = d1, curvature, curvature_first_deriv = d1_curve)

  if(do_plot) {
    g_all <- list(
      ggplot2::geom_line(), ggplot2::xlab(xlab_tag)
    )

  if(smooth) {
    g1 <- ggplot2::ggplot(tracer_data, ggplot2::aes_string(x = colnames(tracer_data)[1],
                                                           y = 'smoothed_tracer')) +
      ggplot2::ylab(ylab_tag) +
      g_all +
      ggplot2::geom_point(ggplot2::aes_string(y = colnames(tracer_data)[2]), pch = 1, cex = 0.7) +
      ggplot2::geom_vline(ggplot2::aes(xintercept = breakthrough_time,
                                       colour = 'breakthrough'), lty = 2) +
      ggplot2::scale_colour_manual(name = ' ', values = (time = 'blue')) +
      ggplot2::theme(legend.position=c(0.2, 0.9),
                     legend.background = ggplot2::element_rect(fill="transparent", colour=NA),
                     legend.key = ggplot2::element_rect(fill = "white")) +
      ggplot2::ggtitle('Tracer breakthrough')
  } else {
    g1 <- ggplot2::ggplot(tracer_data, ggplot2::aes_string(x = colnames(tracer_data)[1],
                                                           y = colnames(tracer_data)[2])) +
      ggplot2::ylab(ylab_tag) +
      g_all +
      ggplot2::geom_vline(ggplot2::aes(xintercept = breakthrough_time,
                                       colour = 'breakthrough'), lty = 2) +
      ggplot2::scale_colour_manual(name = ' ', values = (time = 'blue')) +
      ggplot2::theme(legend.position=c(0.2, 0.9),
                     legend.background = ggplot2::element_rect(fill="transparent", colour=NA),
                     legend.key = ggplot2::element_rect(fill = "white")) +
      ggplot2::ggtitle('Tracer breakthrough')
  }

    g2 <- ggplot2::ggplot(tracer_data, ggplot2::aes_string(x = colnames(tracer_data)[1],
                                                           y = 'curvature')) +
      g_all +
      ggplot2::geom_vline(xintercept = breakthrough_time,
                          colour = 'blue', lty = 2) +
      ggplot2::ylab('curvature') +
      ggplot2::ggtitle('Curvature in time')

    g3 <- ggplot2::ggplot(tracer_data, ggplot2::aes_string(x = colnames(tracer_data)[1],
                                                           y = 'd1_curve')) +
      g_all +
      ggplot2::geom_vline(xintercept = breakthrough_time,
                          colour = 'blue', lty = 2) +
      ggplot2::ylab(expression(paste(Delta, ' curvature'))) +
      ggplot2::ggtitle('Change of curvature (1st derivative)')


    g <- gridExtra::grid.arrange(g1, g2, g3, ncol = 1)

    list(breakthrough_time = as.numeric(breakthrough_time), tracer_data = tracer_data)
  } else list(breakthrough_time = as.numeric(breakthrough_time), tracer_data = tracer_data)
}


#' Calculate stationary flow rate in the drainage
#'
#' @param drainage_data tibble or data.frame. Drainage data from a column experiment. The first column must contain the time, the second the drainage data.
#' @param stationary_time numeric vector of length 2. The time interval where the flow is assumed to be stationary. Typically one would select the shortly before the irrigation is switched off, i.e. 0.9 * end_of_irrigation until end_or_irrigation.
#'
#' @return numeric. The stationary flow rate
#' @export
#'
#' @examples
#' data(drainage)
#' fit_stationary_flow_rate(drainage_data = drainage, stationary_time = c(0.9 * 64410 , 64410))
fit_stationary_flow_rate <- function(drainage_data, stationary_time) {
  ind1 <- max(which(drainage_data[,1] <= stationary_time[2]))
  ind2 <- min(which(drainage_data[,1] >= stationary_time[1]))

  drainage_data[ind1:ind2, ] %>%
    dplyr::pull(var = 2) %>%
    mean(na.rm = T)
}

#' Calculate the theoretical drainage according to the viscous flow approach
#'
#' @description According to the viscous flow approach (e.g. Germann, 2018), the theoretical drainage can be calculated as follows:
#'
#' \loadmathjax
#' \mjdeqn{q(Z,t) = 0 \quad \mathrm{if} \quad T_{B} \leq t \leq T_{W}}{}
#'
#' \mjdeqn{q(Z,t) = q_{S} \quad \mathrm{if} \quad T_{W} \leq t \leq T_{D}}{}
#'
#' \mjdeqn{q(Z,t) = q_{S} \cdot \left( \frac{T_{D} - T_{E}}{t - T_{E}} \right) ^\frac{3}{2} \quad \mathrm{if} \quad T_{D} \leq t \leq \infty}{}
#'
#' where \mjeqn{q(Z,t)}{} is the drainage flow in a depth \mjeqn{Z}{} and time \mjeqn{t}{}, \mjeqn{q_{s}}{} is the flux density applied at the top of the soil column
#' (i.e. irrigation intensity), \mjeqn{T_{B}, T_{E}, T_{W}, T_{D}}{} are the beginning and the end of irrigation, and the arrival times of the wetting and drainage fronts, respectively.
#'
#' @param drainage_data tibble or data.frame. Drainage data from a column experiment. The first column must contain the time, the second the drainage data.
#' @param qS numeric. The volume flux density applied to the top of the soil column (i.e. the irrigation rate or the stationary flow rate, which should be equal at steady state).
#' @param TW numeric. Arrival time of the wetting front.
#' @param TD numeric. Arrival time of the drainage front
#' @param TE numeric. Time of the end of the irrigation.
#' @param exponent numeric. The exponent in the viscous flow calculation. Default is 2/3.
#'
#' @return tibble. The original drainage data with a new column "viscous_flow" with the calculated viscous flow.
#' @export
#'
#' @importFrom Rdpack reprompt
#'
#' @references \insertRef{Germann2018}{ViscousFlow}
#' \insertRef{Bogner2019}{ViscousFlow}
#'
#' @examples
#' data(drainage)
#' flow <- calculate_vf(drainage_data = drainage, qS = 10.1, TE = 64410,
#' TD = 64404 +100, TW = 1000)
calculate_vf <- function(drainage_data, qS, TW, TD, TE, exponent = 3/2) {
  flow_time <- dplyr::pull(drainage_data, var = 1)

  flow <- drainage_data %>%
    dplyr::mutate(
      viscous_flow = dplyr::case_when(
        flow_time < TW ~ 0,
        flow_time < TD & flow_time >= TW ~ qS,
        TRUE ~ qS * ((TD - TE)/(flow_time - TE))^exponent
      )
    )
  flow
}

calculate_linear_reservoir <- function(drainage_data, q0, t0, lambda) {
  flow_time <- dplyr::pull(drainage_data, var = 1)

  flow <- drainage_data %>%
    dplyr::mutate(
      linear_reservoir = dplyr::case_when(
        flow_time < t0 ~ 0,
        TRUE ~ q0 * exp(-lambda * (flow_time - t0))
      )
    )
  flow$linear_reservoir[which(flow_time < t0)] = NA
  flow
}

#' Calculate the flux density at the top of the soil column
#'
#' Converts the pump rate of the irrigation pump into the flux density \mjeqn{q_S}{} in m/s.
#'
#' @param pump_rate numeric. The pump rate in g/min
#' @param D numeric. Diameter of the soil column.
#'
#' @return numeric. The flux density \mjeqn{q_S}{} in m/s
#' @export
calculate_qs <- function(pump_rate, D = 0.08) {
  # pump_rate in g/min, qs in m/h
  pump_rate/((D/2 * 100)^2 * pi * 100 * 60)
}


#' Calculate viscous flow at the end of the drainage for plotting
#'
#' This is an internal function. To calculate the viscous flow use calculate_viscous_flow.
#'
#' @param TD numeric. Arrival time of the drainage front
#' @param qS numeric. Either the flux density at the top of the column or the fitted stationary flow.
#' @param TE numeric. End of irrigation
#' @param time_vector numeric vector. The time vector to calculate the viscous flow.
#' @param exponent numeric. The exponent in the equation of viscous flow. Default is 2/3.
#'
#' @return numeric vector. Viscous flow.
#' @export
#' @NoRd
calc_vf_internal = function(TD, qS, TE, time_vector, exponent = 3/2) {
  res <- sapply(time_vector, function(x) {
    ifelse(x < TD, qS, qS * ((TD - TE)/(x - TE))^exponent)
  })
  res
}


#' Calculate the arrival time of the wetting front
#'
#' @param TD numeric. Arrival time of the drainage front.
#' @param TE numeric. End of irrigation.
#' @param TB numeric. Beginning of irrigation, usually \mjeqn{T_B = 0}{}.
#'
#' @return numeric. Arrival time of the wetting front.
#' @export
calculate_TW = function(TD, TE, TB = 0){
  # check T_D with T_W = T_B + 3*(T_D - T_E)
  TB + 3*(TD - TE)
}


#' Fit the viscous flow equation
#'
#' Fits the viscous flow equation to the drainage curve of an irrigation experiment on a soil column,
#' calculates the arrival time of the drainage front \mjeqn{T_D}{TD}.
#'
#' @param drainage_data tibble or data.frame. Drainage data from a column experiment. The first column must contain the time, the second the drainage data.
#' @param stationary_time numeric vector of length 2. The time interval where the flow is assumed to be stationary. Typically one would select the shortly before the irrigation is switched off, i.e. 0.9 * end_of_irrigation until end_or_irrigation.
#' @param D numeric. Diameter of the soil column in m.
#' @param TE numeric. End of irrigation
#' @param TD_interval numeric vector of length 2. The time interval where the arrival time of the drainage front \mjeqn{T_D}{TD} is expected (initial guess for the optimization routine). Default is NULL and is internally replaced by TD_interval = c(0.9 * TE, 1.1 * TE)
#' @param qS numeric. Stationary flow rate in m/s. Default is NULL and fit_qS = TRUE to fit \mjeqn{q_S}{qS}, cf. fit_stationary_flow_rate.
#' @param fit_qS logical. Should \mjeqn{q_S}{qS} be optimized. Default is TRUE.
#' @param delta_t numeric. The time interval in the drainage_data.
#' @param my_weights numeric or NULL. Weights for the optimization routine. Experimental, at the moment set to 1.
#'
#' @return list.
#' q_ms: drainage in m/s
#' ind: time from which on the fit was done
#' qS: stationary flow rate in m/s
#' TD: arrival time of the drainage front
#' TW: arrival time of the wetting front
#' TE: end of irrigation (unchanged)
#' viscous_flow_tail: theoretical drainage according to the viscous flow used for the optimization
#' @export
#'
#' @examples
#' data(drainage)
#' my_TD <- fit_drainage_tail(drainage_data = drainage, stationary_time = c(0.9 * 64410 , 64410),
#' TE = 64410, TD_interval = c(0.9 * 64410, 1.1 * 64410), qS = NULL, fit_qS = TRUE,
#' delta_t = 30, my_weights = NULL)
fit_drainage_tail <- function(drainage_data, stationary_time, D,
                              TE, TD_interval = NULL,
                              qS = NULL, fit_qS = TRUE, delta_t = 30,
                              my_weights = 1) {

  if(is.null(TD_interval)) TD_interval = c(0.9 * TE, 1.1 * TE)
  # transform q1 from mm/h to m/s
  q_ms <- drainage_data %>%
    dplyr::pull(var = 2)/(1000*3600)
  flow_time <- drainage_data %>%
    dplyr::pull(var = 1)

  # fit flow rate during stationary flow
  if(fit_qS)  qS <- fit_stationary_flow_rate(drainage_data = drainage_data, stationary_time)/(1000*3600)
  else qS <- qS

  # optimize the arrival time of the drainage front
  # exclude all time points until lower time limit in TD_interval
  ind = min(which(flow_time > TD_interval[1]))

  fit_TD = function(theta, weights) {
    sum((weights * (calc_vf_internal(TD = theta, qS = qS, TE = TE, time_vector = flow_time[-c(1:ind)]) - q_ms[-c(1:ind)]))^2)
  }

  if(is.null(my_weights)) {
    weight_1 = max(which(flow_time[-c(1:ind)] <= TE))
    my_weights = c(rep(0.5, weight_1), rep(1.5, (length(flow_time[-c(1:ind)]) - weight_1)))
  }
  TD_optim <- stats::optimize(fit_TD, interval = TD_interval, weights = my_weights, tol = 0.0001)$minimum

  viscous_flow_tail <- calc_vf_internal(TD = TD_optim, qS = qS, TE = TE, flow_time[-c(1:ind)])


  TD = ceiling(TD_optim / delta_t) * delta_t

  TW = calculate_TW(TD = TD, TE = TE)

  # Plot the results
  flow <- data.frame('flow_time' = flow_time[-c(1:ind)], 'flow_experiment' = q_ms[-c(1:ind)],
                 'viscous_flow_tail' = viscous_flow_tail)
  g <- ggplot2::ggplot(data = flow, ggplot2::aes_string(x = 'flow_time', y = 'flow_experiment')) +
    ggplot2::geom_line() +
    ggplot2::geom_line(ggplot2::aes_string(y = 'viscous_flow_tail'), colour = 'orange') +
    ggplot2::xlab('Time') +
    ggplot2::ylab('Flux (m/s)') +
    ggplot2::geom_vline(xintercept = TD, lty = 2, col = 'blue')

  print(g)
  list(q_ms = q_ms, ind = ind, qS = qS, TD = TD, TW = TW, TE = TE, viscous_flow_tail = viscous_flow_tail)
}


#' Calculate viscous flow parameters
#'
#' Calculates the parameters of the viscous flow, namely celerity, film thickness and contact area.
#'
#' @param eta numeric. Cinetamitc viscosity, default is 1E-6.
#' @param g numeric. Gravity constan, default is 9.81 m s\mjeqn{^-2}{}.
#' @param TD numeric. Arrival time of the drainage front.
#' @param TE numeric. End of irrigation.
#' @param Z numeric. Length of the soil column in m.
#' @param qS numeric. The flux density at the top of the soil column (or the fitted stationary flow rate) in m s\mjeqn{^-1}{}.
#'
#' @return data.frame
#' c: celerity
#' F: film_thickness in \mjeqn{\mu}{}m
#' L: contact_area in m \mjeqn{^-1}{}
#'
#' @export
#'
#' @examples
#' vf_params <- calculate_vf_parameters(TD = 64830, TE = 64410,
#' Z = 0.3, qS = 2.79E-06)
calculate_vf_parameters <- function(eta = 1E-6, g = 9.81, TD, TE, Z, qS) {
  celerity <- Z / (TD - TE)
  film_thickness <-(celerity * eta/g)^0.5
  contact_area = 3 * qS * 1/film_thickness^3 * eta/g
  data.frame('c' = celerity, 'F' = film_thickness * 1E6, 'L' = contact_area)
}

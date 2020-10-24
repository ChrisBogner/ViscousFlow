



find_tracer_breakthrough <- function(tracer_data, time_from, time_to, do_plot = TRUE) {

  d1 <- sfsmisc::D1ss(tracer_data[,1], tracer_data[,2])
  d2 <- sfsmisc::D2ss(tracer_data[,1], tracer_data[,2])$y

  # formula for curvature from Wikipedia (https://en.wikipedia.org/wiki/Curvature)
  curvature <- abs(d2)/((1+d1^2)^(3/2))
  d1.curve <- sfsmisc::D1ss(tracer_data[,1], curvature)
  ind.search <- which(tracer_data[,1] > time_from & tracer_data[,1] < time_to)
  res <- tracer_data[,1][which.max(curvature[ind.search]) + ind.search[1] - 1]

  if(do_plot) {
    ggplot2::ggplot(tracer_data, ggplot2::aes_string(x = colnames(tracer_data)[1],
                                                   y = colnames(tracer_data)[2])) +
      ggplot2::geom_line() +
      ggplot2::geom_point() +

                    , d1.curve, type = 'b')
    abline(v = res)
    par(new = T)
    plot(Br.smooth$time, curvature, type = 'b', col = 'red')
    par(new = T)
    plot(Br.smooth, type = 'b', col = 'blue')
  }
  res
}

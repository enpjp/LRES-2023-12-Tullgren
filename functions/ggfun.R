
#' GGFUN Nice GGPLOT2 Outputs
#'
#' @param dat The weather data
#' @param nice.title A nice title for graph and Y axis
#'
#' 
#' @export ggfun
#'
ggfun <- function(dat, nice.title){
  plot.output <- ggplot(data = dat,
                        aes(x = YYYY,
                            y = annual)) +
    geom_point() +
    geom_smooth( se = FALSE, 
                 method = "lm", formula = "y ~ x") +
    geom_line()+ ggtitle( nice.title ) + ylab(nice.title)
  return(plot.output)
}
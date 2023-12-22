#' Get Images For Report
#' 
#' A wrapper for making it easy to include images
#'
#' @param image.path Path to the image
#' @param image.name Name of the image with extension or list.
#'
#' @return Action only
#' @export get.image.for.report
#'
get.image.for.report <- function( image.name, image.path = "images") {
  
#  eco.atlas::jpegs.to.report.format(jpg.path = image.path)
  
  path.to.image <- fs::path(image.path, image.name)
  
  # Images have been standardised to a height of 1000px and 72dpi. All images in
  # the top level directory are included automatically.
  
  set.dpi <- 700

 knitr::include_graphics(path.to.image, dpi = set.dpi )
    
  
}
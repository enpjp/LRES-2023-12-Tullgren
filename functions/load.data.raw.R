#' Load Data Raw
#'
#' @param path.to.raw.data Directory containing raw data.
#'
#' @return raw data in datum triple form
#' @export load.data.raw
#'
load.data.raw <- function(path.to.raw.data = "data-raw"){

  path.to.data <- fs::path( path.to.raw.data) 
  weather.data.triple <- list.files(
    path.to.data,
    pattern = ".rds$", # Just include .rds files.
    full.names = TRUE, # Build paths to files.
    
    recursive = TRUE) %>%  # Read any subdirectories.
    purrr::map_df(readRDS) %>% # Make a dataframe
    rbind()  # Stack all the files found   
  
  return(weather.data.triple)
  
}
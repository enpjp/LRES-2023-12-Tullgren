# Refresh Data Cache

refresh.data.cache <- function(){
  
  path.to.data <-
    fs::path("data-ext", "suttonboningtondata", ext = "txt")
  suttonboningtondata <- get.weather.data.txt(path.to.data)
  
  save.data.as.triple(suttonboningtondata,"suttonboningtondata")
  
  weather.data.triple <- load.data.raw("data-raw")
  
  # Prepare data-sl
  # First prepare the long data
  VC55.weather.sl <- weather.data.triple %>%
    pivot_wider(
      id_cols = datumEntity,
      names_from = datumAttribute,
      values_from = datumValue)
  
  # But we are interesting in plotting the data by date so
  VC55.weather.ss <- create.data.ss.from.sl(VC55.weather.sl)
  
  dir.create( 
    fs::path( 
      "data-ss" ),  
    showWarnings = FALSE,
    recursive = TRUE) # Create the directory
  #And save.
  path.to.save.ss<- fs::path( 
    "data-ss","VC55.weather.ss",
    ext = "rds")
  saveRDS(VC55.weather.ss, path.to.save.ss)
  
  return(invisible())
  
}
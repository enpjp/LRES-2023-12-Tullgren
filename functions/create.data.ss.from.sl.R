
#' Create Data SS from Data SL
#'
#' @param data.sl data-ss in datum triple format
#'
#' @return Data in SS format
#' @export create.data.ss.from.sl
#'
create.data.ss.from.sl <- function(data.sl){
  
  VC55.weather.ss <- data.sl %>% 
    select("YYYYMMDD", 
           "tmax.degC",
           "tmin.degC",
           "airfrost.days", 
           "sun.hours" ,
           "rain.mm")     %>%
    pivot_longer(!YYYYMMDD,
                 names_to = "datumAttribute", 
                 values_to = "datumValue") 
  
  # Specify data types
  VC55.weather.ss$YYYYMMDD <-  
    VC55.weather.ss$YYYYMMDD %>% as.Date
  VC55.weather.ss$datumValue <- 
    VC55.weather.ss$datumValue %>% as.numeric()
  
  return(VC55.weather.ss)
}
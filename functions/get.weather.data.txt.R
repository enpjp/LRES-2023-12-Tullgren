# get.weather.data

#' A function to read the weather data
#'
#' @param path.to.txt 
#' @param skip.rows 
#' @param place.name 
#'
#' @return
#' @export get.weather.data.txt
#'

  get.weather.data.txt <- function(path.to.txt, 
                                   place.name = "Sutton.Bonnington",
                                   skip.rows = 5,
                                   lattitude = 52.833,
                                   longitude = -1.250,
                                   easting = 450700,
                                   northing = 325900,
                                   height.amsl.metre = 48){
    
    
  # expect a txt file
  weatherdata <- read_table(
    path.to.txt,
    skip = skip.rows) 
  
  # Rename the columns to something useful
  new.colnames <- c( "YYYY",
                     "mm",
                     "tmax.degC",
                     "tmin.degC",
                     "airfrost.days",
                     "rain.mm",
                     "sun.hours"
  )
  colnames(weatherdata) <- new.colnames
  
  # Drop row 1
  weatherdata <-  weatherdata[-1,]
  
  # Remove unwanted characters
  
  weatherdata <- weatherdata %>%
    map_df( gsub,
            pattern = "\\*",
            replacement = "") %>%
    map_df( gsub,
            pattern = "---",
            replacement = NA
    )
  
  # Build nominal dates
  
  weatherdata$mm <-   stringr::str_pad(
    weatherdata$mm, width = 2, pad = "0")
  # Create a date at the end of the month.
  dummyDate <- with( weatherdata, 
                     paste( YYYY, 
                            mm, 
                            "28", # Need a dummy day
                            sep= "-"   ) 
  )
  # Calculates the last day of the month
  weatherdata$YYYYMMDD <- 
    timeDate::timeLastDayInMonth(dummyDate) %>% 
    as.character()
  # Create a unique datumEntity for each row
  weatherdata$datumEntity <- with(
    weatherdata,
    paste(place.name, YYYY, mm, sep = ":" )
  )
  
  # manually name some values
  
  weatherdata$lattitude <- lattitude
  weatherdata$longitude <- longitude
  weatherdata$easting <- easting
  weatherdata$northing <- northing
  weatherdata$height.amsl.metre <- height.amsl.metre
  
  
  
  return(weatherdata)
}
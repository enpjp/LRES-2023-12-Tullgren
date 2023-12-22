
#' Nice map of VC55
#'
#'This map has an outline of the Watsonian boundary
#'
#' @param path.to.map location of Watsonian shap file.
#'
#' @return ggmap object
#' @export C55.nice.map
#'
#' 
VC55.nice.map <- function(path.to.map) {
  VC55_sf <- st_read(path.to.map, quiet = TRUE)
  ignore.messages <- capture.output({
    # There is no CRS set so
    sf::st_crs(VC55_sf) <- "EPSG:27700"
    
    VC55_sf <-  sf::st_transform(VC55_sf, "+proj=longlat +datum=WGS84")
    
    
  } )
  
  
  leics.bb <- rbind(
    'x' = c(min =-1.831381, max = -0.1945425 ),
    'y' = c(min = 52.275062, max =53.0947564 ) 
  )  %>% data.matrix() # this makes sure the values are numeric.
  
  
  
  quiet <- function(x) { 
    sink(tempfile()) 
    on.exit(sink()) 
    invisible(force(x)) 
  } 
  
  no.message <- quiet({
    map.overview <- ggmap::get_stamenmap(leics.bb, 
                                         maptype = "terrain", 
                                         messaging	= FALSE,
                                         crop = TRUE)
    
  })
  
  
  
  
  weather.station <- tibble(Long = -1.250, Lat = 52.833)
  
  #  c( Long = -1.250, Lat = 52.833) %>% as.data.frame()
  VC55_poly <- sf::st_coordinates(VC55_sf) %>% as.data.frame()
  
  nice.map <- ggmap::ggmap(map.overview) + 
    geom_polygon(data = VC55_poly, aes(x = X, y= Y), colour="black", fill=NA   )+
    geom_point(data = weather.station, aes(x = Long , y = Lat , size = 3), colour="red") +
    
    #  geom_sf(data=VC55_sf,  aes(geometry = geometry)) +
    # geom_sf(data = VC55_sf) +
    
    theme(legend.position="none")
  
  return(nice.map)
  
}

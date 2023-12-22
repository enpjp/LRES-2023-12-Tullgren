# Z Score



gg.z.score <- function(VC55.weather.ss) {
  
  data.to.plot <- VC55.weather.ss
  data.to.plot <-   data.to.plot %>% drop_na() %>%
    group_by(datumAttribute) %>% 
    mutate( value = datumValue,
            Z.score = 
              (datumValue -mean(datumValue))/sd(datumValue)
    ) %>% select(measurement = datumAttribute,everything())
  
  
  
  plot.out <-   ggplot(data = data.to.plot, aes(colour = measurement, 
                x= YYYYMMDD, y =Z.score, 
                group = measurement) ) + 
    geom_smooth( se = FALSE, 
                 method = "loess", 
                 formula = "y ~ x") + 
    scale_colour_viridis_d()
  
  return(plot.out)

  
  
  
  
  
}
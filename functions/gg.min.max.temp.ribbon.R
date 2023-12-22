# Temperature as a ribbon

gg.min.max.temp.ribbon <- function(VC55.weather.ss, type = "both") {
  
  # For tmax
  temperature.data.rows <- which(grepl("tmax.degC$",VC55.weather.ss$datumAttribute))
  data.to.plot.local <- VC55.weather.ss[temperature.data.rows,]
  data.to.plot.local$YYYY <- format(data.to.plot.local$YYYYMMDD, format = "%Y") %>% as.numeric()
  
  data.to.plot.annual.tmax <- data.to.plot.local %>% drop_na() %>%
    group_by(YYYY) %>%
    mutate( 
      `temperature` = max(datumValue)
    ) %>% select(YYYY,datumAttribute,`temperature`) %>% unique
  
  
  #for.tmin
  
  temperature.data.rows <- which(grepl("tmin.degC$",VC55.weather.ss$datumAttribute))
  data.to.plot.local <- VC55.weather.ss[temperature.data.rows,]
  data.to.plot.local$YYYY <- format(data.to.plot.local$YYYYMMDD, format = "%Y") %>% as.numeric()
  
  data.to.plot.annual.tmin <- data.to.plot.local %>% drop_na() %>%
    group_by(YYYY) %>%
    mutate( 
      `temperature` = min(datumValue)
    ) %>% select(YYYY,datumAttribute,`temperature`) %>% unique
  
  data.to.plot.annual <- rbind(data.to.plot.annual.tmax, data.to.plot.annual.tmin)
  
switch( type,
        
        tmax = {data.to.plot.annual <- data.to.plot.annual.tmax },
        tmin = {data.to.plot.annual <- data.to.plot.annual.tmin  },
        both = {data.to.plot.annual <- rbind(data.to.plot.annual.tmax, 
                                             data.to.plot.annual.tmin) }
  
  
)
  
  
  plot.out <- ggplot(data = data.to.plot.annual,
                        aes(x = YYYY,
                            y = 1)) + 
    
    geom_tile( aes(fill= `temperature`))  +
    scale_fill_viridis( discrete = FALSE, option = "C", direction = 1) + 
    theme_minimal()+
    theme(axis.text.y=element_blank()) +
    ylab("") + facet_grid(rows = vars(datumAttribute))
  return(plot.out)
}
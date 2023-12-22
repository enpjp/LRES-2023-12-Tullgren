#. gg.rainfall.ribbon

gg.rainfall.ribbon <- function(VC55.weather.ss ){
  
  data.to.plot.local <- VC55.weather.ss[VC55.weather.ss$datumAttribute == "rain.mm", ]
  data.to.plot.local$YYYY <- format(data.to.plot.local$YYYYMMDD, format = "%Y") %>% as.numeric()
  
  data.to.plot.annual <- data.to.plot.local %>% drop_na() %>%
    group_by(YYYY) %>%
    mutate( 
      `rainfall mm` = sum(datumValue)
    ) %>% select(YYYY, `rainfall mm`) %>% unique()
  
  plot.output <- ggplot(data = data.to.plot.annual,
                        aes(x = YYYY,
                            y = 1)) + 
    
    geom_tile( aes(fill=`rainfall mm`))  +
    #  scale_y_discrete(drop = FALSE) + 
    #  scale_x_discrete(drop = FALSE) + 
    scale_fill_viridis( discrete = FALSE, option = "D", direction = 1) + 
    theme_minimal()+theme(axis.text.y=element_blank()) +ylab("")
  # coord_fixed()
  
  
  return(plot.output)
  
  
  
}
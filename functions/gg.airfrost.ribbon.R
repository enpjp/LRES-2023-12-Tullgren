#. gg.airfrost.ribbon

gg.airfrost.ribbon <- function(VC55.weather.ss ){
  
  data.to.plot.local <- VC55.weather.ss[VC55.weather.ss$datumAttribute == "airfrost.days", ]
  data.to.plot.local$YYYY <- format(data.to.plot.local$YYYYMMDD, format = "%Y") %>% as.numeric()
  
  data.to.plot.annual <- data.to.plot.local %>% drop_na() %>%
    group_by(YYYY) %>%
    mutate( 
      `airfrost days` = sum(datumValue)
    ) %>% select(YYYY, `airfrost days`) %>% unique()
  
  plot.output <- ggplot(data = data.to.plot.annual,
                        aes(x = YYYY,
                            y = 1)) + 
    
    geom_tile( aes(fill=`airfrost days`))  +
    #  scale_y_discrete(drop = FALSE) + 
    #  scale_x_discrete(drop = FALSE) + 
    scale_fill_viridis( discrete = FALSE, option = "C", direction = -1) + 
    theme_minimal()+theme(axis.text.y=element_blank()) +ylab("")
  # coord_fixed()
  
  
  return(plot.output)
  
  
  
}
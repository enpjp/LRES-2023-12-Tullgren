# Average Annual Temperature

gg.average.annual.temp <- function(VC55.weather.ss){
  
  temperature.data.rows <- which(grepl("degC$",VC55.weather.ss$datumAttribute))
  data.to.plot.local <- VC55.weather.ss[temperature.data.rows,]
  data.to.plot.local$YYYY <- format(data.to.plot.local$YYYYMMDD, format = "%Y") %>% as.numeric()
  
  data.to.plot.annual <- data.to.plot.local %>% drop_na() %>%
    group_by(YYYY) %>%
    mutate( 
      annual = mean(datumValue)
    ) %>% select(YYYY,datumAttribute,annual) %>% unique
  
  lm.model <- with(data.to.plot.annual,lm(annual~YYYY))
  
  sixty.years.change <- round(lm.model$coefficients[2]*60, 2) 
  # 
  # lm.model.residuals <- residuals(lm.model) %>% as_tibble_col("Difference")
  # 
  # data.to.plot.extended <- cbind(data.to.plot.annual,lm.model.residuals )
  
 plot.out <-  ggfun(dat = data.to.plot.annual, nice.title = "" ) 
  
  
  return(plot.out)
  
}
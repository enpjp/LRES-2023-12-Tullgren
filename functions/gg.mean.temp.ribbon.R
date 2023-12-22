# Temperature as a ribbon

gg.mean.temp.ribbon <- function(VC55.weather.ss) {
  
  temperature.data.rows <- which(grepl("degC$",VC55.weather.ss$datumAttribute))
  data.to.plot.local <- VC55.weather.ss[temperature.data.rows,]
  data.to.plot.local$YYYY <- format(data.to.plot.local$YYYYMMDD, format = "%Y") %>% as.numeric()
  
  plot.out <- gg.ribbon(data.to.plot.local, direction=1)
  return(plot.out)
}
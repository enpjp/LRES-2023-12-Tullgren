# Seasonal Rainfall


gg.seasonal.rainfall <- function(VC55.weather.ss) {
  
  temperature.data.rows <- which(grepl("rain.mm$",VC55.weather.ss$datumAttribute))
  data.to.plot.local <- VC55.weather.ss[temperature.data.rows,]
  data.to.plot.local$YYYY <- format(data.to.plot.local$YYYYMMDD, format = "%Y") # %>% as.numeric()
  data.to.plot.local$MM <- format(data.to.plot.local$YYYYMMDD, format = "%m") # %>% as.numeric()
  working.data <- data.to.plot.local
  working.data$rainfall.mm <- working.data$datumValue
  # Create factor of included years
  YYYY.factor <- working.data$YYYY %>% unique() %>% as.character() %>% fct_inseq()
  MM.factor <- c("01","02","03","04","05","06","07","08","09","10","11","12")
  
  working.data$count <- 1
  #temp.data.ag.by.month = aggregate(count ~ YYYY + MM, 
  #                                         data=working.data, sum, na.rm=TRUE)
  
  temp.data.ag.by.month <- working.data
  # Add missing factor levels
  
  #temp.data.ag.by.month$YYYY <- factor(temp.data.ag.by.month$YYYY, YYYY.factor) %>% fct_inseq()
  
  temp.data.ag.by.month$YYYY <- temp.data.ag.by.month$YYYY %>% as.numeric()
  # Addd missing factor values
  temp.data.ag.by.month$MM <- factor(temp.data.ag.by.month$MM, 
                                     levels = MM.factor )
  
  # temp.data.ag.by.month.all$MM <- temp.data.ag.by.month.all$MM %>% as.numeric()
  # temp.data.ag.by.month.all <- temp.data.ag.by.month.all[order(temp.data.ag.by.month.all$MM),]
  
  
  gplotStrip <- ggplot(data = temp.data.ag.by.month, aes(y=MM,x=YYYY))  +
    ggtitle("Seasonal Rainfall") +
    geom_tile(aes(fill= rainfall.mm))  +
    #   scale_y_discrete(drop = FALSE) + 
    #  scale_x_discrete(drop = FALSE) + 
    scale_fill_viridis( discrete = FALSE, option = "D") + 
    theme_minimal() 
  #  theme(axis.text.x  = element_text(angle=45, hjust = 1),
  #        axis.title = element_blank())
  
  
  return(gplotStrip)
  
  
  
  
  
}
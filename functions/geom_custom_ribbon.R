# Geon Ribbon Settings

geom_custom_ribbon <- function(...) {
  
  scale_fill_viridis( discrete = FALSE, option = "C", direction = direction) + 
    theme_minimal()+
    theme(axis.text.y=element_blank()) +
    ylab("") 
  
  
  
}
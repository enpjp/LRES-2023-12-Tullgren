#' GG Ribbon
#'
#' @param data.to.plotThe data
#' @param YYYY The x-axis
#' @param direction legend fill direction

#'
#' @return
#' @export
#'
#' @examples
gg.ribbon <- function(data.to.plot, direction = 1){
  
  data.to.plot.annual <- data.to.plot %>% drop_na() %>%
    group_by(YYYY) %>%
    mutate( 
      `mean temp` = mean(datumValue)
    ) %>% select(YYYY,datumAttribute,`mean temp`) %>% unique
  
  plot.output <- ggplot(data = data.to.plot.annual,
         aes(x = YYYY,
             y = 1)) + 
     geom_tile( aes(fill= `mean temp`))  +
    scale_fill_viridis( discrete = FALSE, option = "C", direction = direction) +
    theme_minimal()+
    theme(axis.text.y=element_blank()) +
    ylab("")
    # 
    

  return(plot.output)

  }
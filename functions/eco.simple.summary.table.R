
# Eco.summary.table

# Makes really nice summary tables of records

eco.simple.summary.table <- function(path.to.data.ss) {
  
  
  my.data.ss <- readRDS(path.to.data.ss)
  
 # columns.to.keep <- c("order","family","taxon", "common.name","gender")
  columns.to.keep <- c("order","family","taxon", "common.name")
  
  
  my.species.present <- my.data.ss[,columns.to.keep] %>% 
    arrange( order, family, taxon) %>% unique()
  
  unfill_vec <- function(x) {
    same <- x == dplyr::lag(x)
    ifelse(!is.na(same) & same, " " , x)
  }
  
  my.species.present$order <-     unfill_vec(my.species.present$order)
  my.species.present$family <-     unfill_vec(my.species.present$family)
  
  # Nice column names
  
  colnames( my.species.present) <- c("Order","Family","Taxon", "Common Name")
 
 my.table <- knitr::kable( my.species.present, caption = 'Summary of taxa recorded.',
              booktabs = TRUE,
              longtable = TRUE,
              escape=F,
              format="latex") %>%  kable_styling(latex_options = "HOLD_position") %>%
             kable_styling(latex_options = "repeat_header") %>%
              kable_styling(font_size = 7)


  
  return(my.table)
}
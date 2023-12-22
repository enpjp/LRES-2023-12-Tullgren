#' Save data in datumTriple format
#'
#' @param data 
#' @param path.to.save 
#'
#' @return
#' @export save.data.as.triple
#'
save.data.as.triple <- function(data, path.to.save){
  
    triple <- data %>% 
    map_df(as.character) %>% # Convert to character
    pivot_longer(!datumEntity, # datumEntity as  key
                 names_to = "datumAttribute", # Attribute name
                 values_to = "datumValue" # Save  value
    ) %>% drop_na() # Drop NAs
  
    saveRDS(triple,path.to.save)
}
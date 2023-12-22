#' Save data in datumTriple format
#'
#' @param data 
#' @param path.to.save 
#'
#' @return
#' @export save.raw.data
#'
save.raw.data <- function(data, file.to.save){
    path.to.save <- fs::path("data-raw", file.to.save, ext = "rds")
  
    triple <- data %>% 
    map_df(as.character) %>% # Convert to character
    pivot_longer(!datumEntity, # datumEntity as  key
                 names_to = "datumAttribute", # Attribute name
                 values_to = "datumValue" # Save  value
    ) %>% drop_na() # Drop NAs
    
  
    saveRDS(triple,path.to.save)
}
#' Turn data into datumTriple format
#'
#' @param data 
#' 
#'
#' @return data as triple
#' @export data.as.triple
#'
data.as.triple <- function(data){
  
    triple <- data %>% 
    map_df(as.character) %>% # Convert to character
    pivot_longer(!datumEntity, # datumEntity as  key
                 names_to = "datumAttribute", # Attribute name
                 values_to = "datumValue" # Save  value
    ) %>% drop_na() # Drop NAs
  
    return(triple )
}
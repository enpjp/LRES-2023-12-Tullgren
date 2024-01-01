
# Eco.summary.table

# Makes really nice summary tables of records

eco.summary.table <- function(path.to.data.ss) {
  
  
  my.data.ss <- readRDS(path.to.data.ss)
  
 # columns.to.keep <- c("order","family","taxon", "common.name","gender")
  columns.to.keep <- c("order","family","taxon", "common.name")
  
  my.species.present <- my.data.ss[,columns.to.keep]
  
 # my.species.present$gender <- my.species.present$gender %>% unlist()
  
  #my.species.present$num <- 1
  #my.species.present.gender <- pivot_wider(data = my.species.present, names_from =  gender)
  
  # my.species.summary <-   my.species.present %>% group_by(family, taxon, gender) %>% 
  #   tally(name = "Count") %>% arrange(family,taxon)
  
  my.species.summary <-   my.species.present %>% group_by(family, taxon) %>% 
    tally(name = "Count") %>% arrange(family,taxon)
  
#  my.species.summary$Figure <- "Fig:"
  
  pre.calculate.rows <- my.species.present %>% group_by(family, taxon)
  
  
  # A new way to do this
  my.species.summary.II <-  bind_rows(
    pre.calculate.rows %>% 
      tally(name = "Count"),
    my.species.present %>% group_by(family) %>% 
      tally(name = "FamilyCount") 
    
  ) %>% group_by(family, taxon) %>% arrange( family, -FamilyCount,  taxon) %>% ungroup()
  
  my.species.summary.II$index <- my.species.summary.II$FamilyCount
  
  my.species.summary.II <- my.species.summary.II %>% fill(index) %>% arrange( -index, family, taxon)
  
  my.species.summary.II$index <- NULL
  
  my.species.summary <- my.species.summary.II
  
  
  # Substitute some NA values
  
  my.species.summary$taxon <- replace_na(my.species.summary$taxon, " Number of records for this family:")
  
  # my.species.summary$Figure <-  paste("Fig: ",
  #                                     "\\ref{fig:dashboard",
  #                                     make.names(my.species.summary$taxon), "}",  sep = ""   )
  # my.species.summary$Figure <- gsub("\\.","", my.species.summary$Figure)
  # 
  # Now it's working blank the values that are not needed
  
  # Which rows end with fullstops
  
  rows.with.stops <- which(grepl("\\.$",my.species.summary$taxon))
#  my.species.summary$Figure[rows.with.stops] <- NA
  
  rows.with.na <- which(is.na(my.species.summary$Count))
#  my.species.summary$Figure[rows.with.na] <- NA
  
  rows.with.unknown <- which(grepl("Unknown",my.species.summary$taxon))
 # my.species.summary$Figure[rows.with.unknown] <- NA
  
  my.species.summary$family <- factor(my.species.summary$family, unique(my.species.summary$family))
  
  my.species.summary.nice.group <- my.species.summary
  my.species.summary.nice.group$family <- NULL
  
 # my.species.summary.checklist <- my.species.summary.nice.group
  
  # Now add some extra data from Pantheon
  # 
  # pantheon.to.add <- pantheon.data %>% select(taxon = Species, habitat = Habitat)
  # 
  # pantheon.to.add$habitat <- gsub("&","and",pantheon.to.add$habitat )
  # 
  # my.species.summary.pantheon <- left_join(my.species.summary.nice.group, pantheon.to.add)
  # 
  # # Now add BAS Checklist
  # 
  # BAS.checklist.fields <- complete.vc55.spider.data.with.checklist %>%
  #   select(taxon =species, 
  #          BAS.checklist = member.of.list) %>%
  #   sf::st_drop_geometry() %>%
  #   unique()
  # BAS.checklist.fields$BAS.checklist <-  gsub("List A1. Species established in natural or semi-natural habitats.",
  #                                             "List A1. Established species.", BAS.checklist.fields$BAS.checklist )
  # 
  # my.species.summary.checklist <- left_join(my.species.summary.pantheon, 
  #                                           BAS.checklist.fields)
 
  # my.species.vernacular <- my.species.present %>% select(taxon, 'common name' = vernacular)
  # my.species.vernacular$`common name` <- gsub("NULL"," ", my.species.vernacular$`common name` )
  # 
  
 
  
   my.species.summary.checklist <- my.species.summary.nice.group %>% ungroup()
   
 #  my.species.summary.checklist <- left_join( my.species.summary.ungroup, my.species.vernacular)
  

  # Now re-order the columns
  col.order <- c("taxon", 
                 "Count", 
                 # "gender",
                 "FamilyCount"
            
  )
  
  my.species.summary.checklist <- my.species.summary.checklist[, col.order]
  
  # Drop rows where family = Family
  #rows.to.keep <- !grepl("Family",my.species.summary$family )
  #my.species.summary.clean <- my.species.summary[rows.to.keep,]
  options(knitr.kable.NA = '')
  my.table <- knitr::kable(my.species.summary.checklist, caption = 'Species recorded by family.',
               booktabs = TRUE, 
               longtable = TRUE,
               escape=F,
               format="latex") %>%  kable_styling(latex_options = "HOLD_position") %>%
              kable_styling(latex_options = "repeat_header") %>%  
               kable_styling(font_size = 7) %>% pack_rows(index = table(my.species.summary$family))
  rows.to.keep <- !grepl("Unknown",my.species.summary$family )
  my.species.summary.ID <- my.species.summary[rows.to.keep,]
  
  return(my.table)
}
countyText <- function(data) {
  
  data.Text <- NULL
  
  for(i in 1:nrow(data)) {
    
    data.Text <- paste0(data$Discipline, " Awardees: ", data$n, " & ", "Funds Awarded: ", scales::dollar(data$funds), "<br>", sep = " ")
  }
  
  data.Text <- list(data.Text)
  
  firstPart <- paste0("County: ", data$PC[i], "<br>",
                      "Tier Level: ", data$Tier.Level, "<br>")
  
  lastPart <- paste0("<b>Total Number of Awardees: ", sum(data$n), " & ", "Total Funds Awarded: ", scales::dollar(sum(data$funds)), "</b>")
  
  countytext <- paste0(firstPart,
                       data.Text,
                       lastPart)
  
  countytext <- str_remove_all(countytext, "\"")
  countytext <- str_remove_all(countytext, "c\\(")
  countytext <- str_remove_all(countytext, "\\)")
  countytext <- str_remove_all(countytext, ", ")
  
  return(countytext)
  
}


grad_text <- function(data) {
  
  data.Text <- NULL
  
  for(i in 1:nrow(data)) {
    
    data.Text <- paste0(data$Discipline, " Graduates: ", data$n, "<br>")
  }
  
  data.Text <- list(data.Text)
  
  firstPart <- paste0("County: ", data$County[i], "<br>")
  
  lastPart <- paste0("<b>Total Number of Graduates Practicing PC more than 50% of the time: ", sum(data$n), "</b>")
  
  countytext <- paste0(firstPart,
                       data.Text,
                       lastPart)
  
  countytext <- str_remove_all(countytext, "\"")
  countytext <- str_remove_all(countytext, "c\\(")
  countytext <- str_remove_all(countytext, "\\)")
  countytext <- str_remove_all(countytext, ",")
  
  return(countytext)
  
}


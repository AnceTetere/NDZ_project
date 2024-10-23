adj50 <- function(NDZ) {
  
  if (NDZ$period[1] == "202102") {
    NDZ <- NDZ %>% 
      filter(!(PS_code == '___________' & NM_code == '___________' & zinkod == '51'))
  }
  
  return(NDZ) 
}

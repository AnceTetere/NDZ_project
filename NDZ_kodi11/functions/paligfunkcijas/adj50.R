adj50 <- function(NDZ) {
    
 if (isTRUE(NDZ$period[1] == "202102")) {
    
    NDZ <- NDZ %>% 
      filter(!(PS_code %in% c('___________', '___________', '___________', '___________') & NM_code == '___________' & zinkod == '51'))
    
    NDZ <- NDZ %>% 
      filter(!(PS_code %in% c('___________', '___________') & NM_code == c('___________', '___________') & zinkod == '41'))

    NDZ <- NDZ %>% 
      filter(!(PS_code == '___________' & NM_code == '___________' & zinkod == '25'))
    
    NDZ <- NDZ %>% 
      filter(!(PS_code == '___________' & NM_code == '___________' & zinkod == '92'))  
    
  } else if (isTRUE(NDZ$period[1] == "202107")) {
    NDZ <- NDZ %>% 
      filter(!(PS_code == '___________' & NM_code == '___________' & zinkod == '51'))
    
  } else if (isTRUE(NDZ$period[1] == "202201" & NDZ$zinkod[1] == "40")) {
        
    NDZ <- NDZ %>% 
      filter(!(PS_code == '___________' & NM_code == '___________')) 
    NDZ[NDZ$PS_code == '___________', ]
    
  } else if (isTRUE(NDZ$period[1] == "202307" & NDZ$zinkod[1] == "40")) {
    NDZ <- NDZ %>% 
      filter(!(PS_code == '___________' & NM_code == '___________' & zinkod == '25'))
  }
  return(NDZ) 
}

changes_202302 <- function(NDZ, kods) {
  
  if (kods == "11") {
    NDZ <- NDZ %>% 
      dplyr::filter(!(
        (PS_code ==  '______________' & NM_code ==  '______________' & NDZ_sanemsanas_datums == '2023-02-15' & zinkod == '25') |
        (PS_code ==  '______________' & NM_code ==  '______________' & NDZ_sanemsanas_datums == '2023-02-08' & zinkod == '26') |
        (PS_code ==  '______________' & NM_code ==  '______________' & NDZ_sanemsanas_datums == '2023-02-08' & zinkod == '26')  
        ))
  } else if (kods == '50') {
    NDZ <- NDZ %>% 
      dplyr::filter(!(PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '51' & NDZ_sanemsanas_datums == '2023-02-18'))
  }
  
  return(NDZ)
}

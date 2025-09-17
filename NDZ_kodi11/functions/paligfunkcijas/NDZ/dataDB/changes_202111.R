 changes_202111 <- function(NDZ, kods) {
  
  if (kods == "11") {
    
    NDZ <- NDZ %>% 
      dplyr::filter(!(
        (PS_code ==  '______________' & NM_code ==  '______________' & NDZ_sanemsanas_datums == '2021-11-02') |
        (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '11' & NDZ_sanemsanas_datums == "2021-11-02") |
        (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '11' & NDZ_sanemsanas_datums == "2021-11-22") |
        (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '11' & NDZ_sanemsanas_datums == "2021-11-15")  
      ))
    
  } else if (kods == "50") {
    
    NDZ <- NDZ %>% 
      dplyr::filter(!(
        (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '50' & NDZ_sanemsanas_datums == "2021-11-16") | 
        (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '51' & NDZ_sanemsanas_datums == "2021-11-23") | 
        (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '51' & NDZ_sanemsanas_datums == "2021-11-23") |
        (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '50' & NDZ_sanemsanas_datums == '2021-11-03') 
       ))
  }
  
  return(NDZ)
}

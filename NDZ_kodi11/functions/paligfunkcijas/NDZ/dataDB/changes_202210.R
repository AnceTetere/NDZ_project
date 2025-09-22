changes_202210 <- function(NDZ, kods) {
  
  if (kods == "11") {
    NDZ <- NDZ %>% 
      dplyr::filter(!(
        (PS_code ==  '______________'& NM_code ==  '______________' & zinkod == '11' & NDZ_sanemsanas_datums == "2022-10-01") |
        (PS_code ==  '______________'& NM_code ==  '______________' & zinkod == '21' & NDZ_sanemsanas_datums == "2022-10-17") |
        (PS_code ==  '______________'& NM_code ==  '______________' & zinkod == '25' & NDZ_sanemsanas_datums == "2022-10-10") |
        (PS_code ==  '______________'& NM_code ==  '______________' & zinkod == '25' & NDZ_sanemsanas_datums == "2022-10-10") 
      ))
    
  } else if (kods == "50") {
    NDZ <- NDZ %>% 
      dplyr::filter(!(
        (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '50' & NDZ_sanemsanas_datums == "2022-10-27") |
        (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '51' & NDZ_sanemsanas_datums == "2022-10-15") |
        (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '50' & NDZ_sanemsanas_datums == "2022-10-18")   
        ))
  }
  
  return(NDZ)
}

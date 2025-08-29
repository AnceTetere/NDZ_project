changes_202208 <- function(NDZ, kods) {

if (kods == "11") {
  
  NDZ <- NDZ %>% 
            dplyr::filter(!(
                    (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '11' & NDZ_sanemsanas_datums == "2022-08-27") |  
                    (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '11' & NDZ_sanemsanas_datums == "2022-08-30") |  
                    (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '11' & NDZ_sanemsanas_datums == "2022-08-20") |  
                    (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '11' & NDZ_sanemsanas_datums == "2022-08-18") |  
                    (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '11' & NDZ_sanemsanas_datums == "2022-08-20")   
                  ))
  
} else if (kods == "40") {
  
  NDZ <- NDZ %>% 
    dplyr::filter(!(
      (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '92' & NDZ_sanemsanas_datums == "2022-08-01") | 
      (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '40' & NDZ_sanemsanas_datums == "2022-08-13") | 
      (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '92' & NDZ_sanemsanas_datums == "2022-08-29")    
    ))
  
} else if (kods == "50") {
  
  NDZ <- NDZ %>% 
    dplyr::filter(!(
      (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '51' & NDZ_sanemsanas_datums == "2022-08-01") | 
      (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '51' & NDZ_sanemsanas_datums %in% c("2022-08-15", "2022-08-23", "2022-08-27")) | 
      (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '51' & NDZ_sanemsanas_datums %in% c("2022-08-14", "2022-08-15")) | 
      (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '51' & NDZ_sanemsanas_datums %in% c("2022-08-05", "2022-08-15", "2022-08-29")) | 
      (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '51' & NDZ_sanemsanas_datums %in% c("2022-08-12", "2022-08-16", "2022-08-17")) | 
      (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '50' & NDZ_sanemsanas_datums == "2022-08-15") | 
      (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '51' & NDZ_sanemsanas_datums == "2022-08-22") | 
      (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '51' & NDZ_sanemsanas_datums == "2022-08-08") | 
      (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '51' & NDZ_sanemsanas_datums == "2022-08-01") | 
      (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '51' & NDZ_sanemsanas_datums == "2022-08-30") | 
      (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '50' & NDZ_sanemsanas_datums == "2022-08-30") | 
      (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '50' & NDZ_sanemsanas_datums == "2022-08-18")   
        ))
  
}
  
  return(NDZ)
} 


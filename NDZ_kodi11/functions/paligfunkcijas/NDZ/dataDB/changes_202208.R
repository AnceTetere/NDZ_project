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
    dplyr::filter(!(PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '92' & NDZ_sanemsanas_datums == "2022-08-01"))
  
} else if (kods == "50") {
  NDZ <- NDZ %>% 
    dplyr::filter(!(PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '51' & NDZ_sanemsanas_datums == "2022-08-01"))
  
}
  
  return(NDZ)
}

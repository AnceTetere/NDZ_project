 changes_202112 <- function(NDZ, kods) {
  
  if (kods == "11") {
    
    NDZ <- NDZ %>% 
      dplyr::filter(!(
          (PS_code ==  '______________' & NM_code ==  '______________' & NDZ_sanemsanas_datums == '2021-12-15')  | 
          (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '11' & NDZ_sanemsanas_datums == "2021-12-14") | 
		  (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '11' & NDZ_sanemsanas_datums == "2021-12-30") |
		  (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '11' & NDZ_sanemsanas_datums == "2021-12-30") | 
          (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '11' & NDZ_sanemsanas_datums == "2021-12-30") |
		  (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '11' & NDZ_sanemsanas_datums == "2021-12-14") |      
		  (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '11' & NDZ_sanemsanas_datums == "2021-12-29") |      
		  (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '11' & NDZ_sanemsanas_datums == "2021-12-30") | 
          (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '11' & NDZ_sanemsanas_datums == "2021-12-14") | 
          (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '11' & NDZ_sanemsanas_datums == "2021-12-24")   
      ))
    
  } else if (kods == "50") {
    
      NDZ <- NDZ %>% 
        dplyr::filter(!(
          (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '50' & NDZ_sanemsanas_datums == '2021-12-09') |#Te indivīds iet bezalgas atvaļinājumā novembrī un tad, bez atgriešanās, iet decembrī, tad janvārī. Man nav vairāk informācijas par novembra un decembra aiziešanas kodiem, tāpēc uzskatu tos par maldinošiem.
          (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '51' & NDZ_sanemsanas_datums == '2021-12-14') #Indivīds atgriežas nākamajā gadā - te kods maldina. Meklēt uz priekšu.
         ))
  }
  return(NDZ)
}

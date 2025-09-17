changes_202108 <- function(NDZ, kods) {
  
  if (kods == "11") {
    
  NDZ <- NDZ %>% 
    dplyr::filter(!(PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '11' & NDZ_sanemsanas_datums == '2021-08-02')) #Jo indivīds tiek pieņemts darbā un atlaists un tad līdzīgi uz menesi jūlijā nākamajā gadā pieņemts un atlaists. Tam kodam 11, nav loģikas - jo ja jau cilvēks ir nodarbinātībā, tad kāpēc tas tiek pieņemts atkal darbā nākamgad. Protams, ka varu kļūdīties.
  
  
  } else if (kods == "50") {
    
    NDZ <- NDZ %>% 
      dplyr::filter(!(
        (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '51' & NDZ_sanemsanas_datums == "2021-08-31") |
        (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '50' & NDZ_sanemsanas_datums == "2021-08-20") #Šim kodam nav loģikas.
      ))
  }
  
  return(NDZ)
}

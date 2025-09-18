changes_202209 <- function(NDZ, kods) {
  
  if (kods == "11") {
    
    NDZ <- NDZ %>% 
      dplyr::filter(!(
        (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == "11" & NDZ_sanemsanas_datums == "2022-09-19")
      ))
    
  } else if (kods == "50") {
    NDZ <- NDZ %>%
      mutate(
        zinkod = if_else(
          PS_code ==  '______________' &
            NM_code ==  '______________' &
            zinkod == '51' &
            NDZ_sanemsanas_datums == as.Date('2022-09-01'),
          '50',
          zinkod
        ),
        sak_beidz = if_else(
          PS_code ==  '______________' &
            NM_code ==  '______________' &
            zinkod == '50' &
            sak_beidz == '1' &
            NDZ_sanemsanas_datums == as.Date('2022-09-01'),
          '2',
          sak_beidz
        )
      )    
    
    NDZ <- NDZ %>% 
      dplyr::filter(!(
          (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == "51" & NDZ_sanemsanas_datums == "2022-09-05") |
          (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == "51" & NDZ_sanemsanas_datums == "2022-09-22") |
          (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == "51" & NDZ_sanemsanas_datums == "2022-09-22") 
          ))
  }
  
  return(NDZ)
}

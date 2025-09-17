changes_202206 <- function(NDZ, kods) {
  
  if (kods == "11") {
             
             NDZ <- NDZ %>% 
               dplyr::filter(!(
                 (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '11' & NDZ_sanemsanas_datums == "2022-06-01") | 
                 (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '11' & NDZ_sanemsanas_datums == "2022-06-28") | 
                 (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '11' & NDZ_sanemsanas_datums == "2022-06-23") | 
                 (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '11' & NDZ_sanemsanas_datums == "2022-06-01") | 
                 (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '11' & NDZ_sanemsanas_datums == "2022-06-24") |  
                 (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '11' & NDZ_sanemsanas_datums == "2022-06-21")    
                ))
  
                
    
  } else if (kods == "40") {
              NDZ <- NDZ %>% 
                mutate(
                  zinkod = ifelse(
                    PS_code  %in%  c'______________', '______________' &
                      NM_code  %in%  c'______________', '______________' &
                      zinkod == '40' &
                      NDZ_sanemsanas_datums %in% c(as.Date('2022-06-28'), as.Date('2022-06-15')),
                    '41', 
                    zinkod),
                  sak_beidz = ifelse(
                    PS_code  %in%  c'______________', '______________' &
                      NM_code  %in%  c'______________', '______________' &
                      zinkod == '41' &
                      NDZ_sanemsanas_datums %in% c(as.Date('2022-06-28'), as.Date('2022-06-15')),
                    '1',
                    sak_beidz
                  ))
              
              NDZ <- NDZ %>% 
                dplyr::filter(!(PS_code ==  '______________' & NM_code ==  '______________' & zinkod %in% c('41', '92') & NDZ_sanemsanas_datums == "2022-06-01"))

  } else if (kods == "50") {
    
             
             NDZ <- NDZ %>% 
               dplyr::filter(!(
                 (PS_code ==  '______________'& NM_code ==  '______________' & zinkod == '51' & NDZ_sanemsanas_datums == "2022-06-01") | #Te un nākošajā mēnesī, kodam 51 nav jēga, jo iepriekšējā mēnesī darbinieks jau ir atlaists.
                 (PS_code ==  '______________'& NM_code ==  '______________' & zinkod == '50' & NDZ_sanemsanas_datums == "2022-06-01")   #Te indivīds aiziet bezalgas atvaļinājumā nākamajā mēnesī, neatgriežoties no šī. Par cik šis kods nav izskaidrojams un atstājot maldina. pieņemu lēmumu to izņemt.
                ))
             
             NDZ <- NDZ %>% 
               mutate(zinkod = 
                        if_else(PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '51' &
                                  NDZ_sanemsanas_datums == as.Date('2022-06-30'), '50', zinkod))
             NDZ <- NDZ %>%
               mutate(sak_beidz = 
                        if_else (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '50' & 
                                   NDZ_sanemsanas_datums == as.Date('2022-06-30') & sak_beidz == '1', '2', sak_beidz))
             
             NDZ <- NDZ %>% 
               dplyr::filter(!(
                 (PS_code ==  '______________'& NM_code ==  '______________' & zinkod == '51' & NDZ_sanemsanas_datums == "2022-06-01")  līdz ar to, tas maldina.
               ))
    
  } else if (kods == "53") {
              NDZ <- NDZ %>% 
                dplyr::filter(!(PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '54' & NDZ_sanemsanas_datums == "2022-06-04"))
              
              NDZ <- NDZ %>% 
                dplyr::filter(!(PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '54' & NDZ_sanemsanas_datums == "2022-06-01"))
  }
  
  return(NDZ)
} 

adj50 <- function(NDZ) {
    
 if (isTRUE(NDZ$period[1] == "_____")) {
    NDZ <- NDZ %>% 
      dplyr::filter(!(PS_code %in% c('__________', '__________', '__________', '__________') & NM_code == '__________' & zinkod == '51'))
    
    NDZ <- NDZ %>% 
      dplyr::filter(!(PS_code %in% c('__________', '__________') & NM_code == c('__________', '__________') & zinkod == '41'))

    NDZ <- NDZ %>% 
      dplyr::filter(!(PS_code == '__________' & NM_code == '__________' & zinkod == '25'))
    
    NDZ <- NDZ %>% 
      dplyr::filter(!(PS_code == '__________' & NM_code == '__________' & zinkod == '92'))  

 } else if (isTRUE(NDZ$period[1] == "_____")) {
   NDZ <- NDZ %>% 
     dplyr::filter(PS_code == '__________' & NM_code == '__________' & zinkod == '11') %>% 
     mutate(zinkod = '50')

 } else if (isTRUE(NDZ$period[1] == "_____" & kods == "11")) {
   NDZ <- NDZ %>% 
     dplyr::filter(!(PS_code == '__________' & NM_code == '__________' & NDZ_sanemsanas_datums == '2021-10-12'))

 } else if (isTRUE(NDZ$period[1] == "_____" & kods == "11")) {
   NDZ <- NDZ %>% 
     dplyr::filter(!(PS_code == '__________' & NM_code == '__________' & NDZ_sanemsanas_datums == '2021-11-02'))
   
 } else if (isTRUE(NDZ$period[1] == "_____") && kods == "11") {
   NDZ <- NDZ %>% 
     dplyr::filter(PS_code == '__________' & NM_code == '__________' & zinkod == '29') %>% 
     mutate(zinkod = '40')
   
  } else if (isTRUE(NDZ$period[1] == "_____" & kods == "50")) {
    NDZ <- NDZ %>% 
      dplyr::filter(!(PS_code == '__________' & NM_code == '__________' & zinkod == '51'))
    
  } else if (isTRUE(NDZ$period[1] == "_____" & kods == "11")) {
    NDZ <- NDZ %>% 
      dplyr::filter(!(PS_code == '__________' & NM_code == '__________' & zinkod == '11' & NDZ_sanemsanas_datums == "2021-10-12"))

  } else if (isTRUE(NDZ$period[1] == "_____" & kods == "11")) {
    NDZ <- NDZ %>% 
      dplyr::filter(!(PS_code == '__________' & NM_code == '__________' & zinkod == '11' & NDZ_sanemsanas_datums == "2021-11-02"))
    
    
    } else if (isTRUE(NDZ$period[1] == "_____" & kods == "40")) {
      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code == '__________'& NM_code == '__________' & zinkod == '91' & NDZ_sanemsanas_datums == "2022-02-01"))

      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code == '__________'& NM_code == '__________' & zinkod == '40' & NDZ_sanemsanas_datums == "2022-02-16"))
      
      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code == '__________'& NM_code == '__________' & zinkod == '91' & NDZ_sanemsanas_datums == "2022-02-01"))
    
    } else if (isTRUE(NDZ$period[1] == "_____") && kods == "11") {
      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code == '__________'& NM_code == '__________' & zinkod == '11' & NDZ_sanemsanas_datums == "2022-02-21"))
      
      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code == '__________'& NM_code == '__________' & zinkod == '11' & NDZ_sanemsanas_datums == "2022-02-22"))
      
      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code == '__________'& NM_code == '__________' & zinkod == '25' & NDZ_sanemsanas_datums == "2022-02-01"))
   
    } else if (isTRUE(NDZ$period[1] == "_____" & kods == "11")) {
      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code == '__________'& NM_code == '__________' & zinkod == '11' & NDZ_sanemsanas_datums == "2022-03-07"))
      
      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code == '__________'& NM_code == '__________' & zinkod == '11' & NDZ_sanemsanas_datums == "2022-03-12"))
      
      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code == '__________'& NM_code == '__________' & zinkod == '11' & NDZ_sanemsanas_datums == "2022-03-27"))

    } else if (isTRUE(NDZ$period[1] == "_____" & kods == "40")) {
      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code == '__________' & NM_code == '__________' & zinkod == '92' & NDZ_sanemsanas_datums == "2022-03-21"))

    } else if (isTRUE(NDZ$period[1] == "_____" & kods == "50")) {
      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code == '__________' && NM_code == '__________' & zinkod == '51' & NDZ_sanemsanas_datums == "2022-03-31"))

    } else if (isTRUE(NDZ$period[1] == "_____" & kods == "11")) {
      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code == '__________'& NM_code == '__________' & zinkod == '11' & NDZ_sanemsanas_datums == "2022-04-05"))
      
    } else if (isTRUE(NDZ$period[1] == "_____" & kods == "40")) {
      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code == '__________'& NM_code == '__________' & zinkod == '91' & NDZ_sanemsanas_datums == "2022-04-21"))
      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code == '__________'& NM_code == '__________' & zinkod == '91' & NDZ_sanemsanas_datums == "2022-04-21"))
      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code == '__________'& NM_code == '__________' & zinkod == '92' & NDZ_sanemsanas_datums == "2022-04-30"))
      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code == '__________'& NM_code == '__________' & zinkod == '92' & NDZ_sanemsanas_datums == "2022-04-30"))
      
    } else if (isTRUE(NDZ$period[1] == "_____" & kods == "50")) {
      #Te dziļāka izpēte rāda, ka kods 51 ir kļūda.
      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code == '__________'& NM_code == '__________' & zinkod == '51' & NDZ_sanemsanas_datums == "2022-04-30"))
      
      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code == '__________'& NM_code == '__________' & zinkod == '50' & NDZ_sanemsanas_datums == "2022-04-16"))

      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code == '__________'& NM_code == '__________' & zinkod == '51' & NDZ_sanemsanas_datums == "2022-04-01"))

    } else if (isTRUE(NDZ$period[1] == "_____" & kods == "53")) {
      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code == '__________'& NM_code == '__________' & zinkod == '53' & NDZ_sanemsanas_datums == "2022-04-30"))
  
    } else if (isTRUE(NDZ$period[1] == "_____") && kods == "50") {
      
      NDZ <- NDZ %>% 
        mutate(
          zinkod = ifelse(
            PS_code == '__________' &
              NM_code == '__________' &
              zinkod == '51' &
              NDZ_sanemsanas_datums == as.Date('2022-05-23'),
            '50',
            zinkod
          ),
          sak_beidz = ifelse(
            PS_code == '__________' &
              NM_code == '__________' &
              zinkod == '50' &
              NDZ_sanemsanas_datums == as.Date('2022-05-23'),
            '2',
            sak_beidz
          )
        )
      
      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code == '__________'& NM_code == '__________' & zinkod == '51' & NDZ_sanemsanas_datums == "2022-05-04"))

    } else if (isTRUE(NDZ$period[1] == "_____" & kods == "53")) {
      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code == '__________'& NM_code == '__________' & zinkod == '53' & NDZ_sanemsanas_datums == "2022-05-05"))
      
       
    } else if (isTRUE(NDZ$period[1] == "_____" & kods == "40")) {
      NDZ <- NDZ %>%
        mutate(
          zinkod = ifelse(
            PS_code == '__________' &
            NM_code == '__________' &
            zinkod == '51' &
            NDZ_sanemsanas_datums == as.Date('2022-07-01'), 
          '41', 
          zinkod))

    } else if (isTRUE(NDZ$period[1] == "_____" & kods == "11")) {
      
      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code == "__________" & NM_code == "__________" & zinkod == "25" & NDZ_sanemsanas_datums == "2022-07-12"))
      
    } else if (isTRUE(NDZ$period[1] == "_____") && kods == "11") {
      
      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code == '__________'& NM_code == '__________' & zinkod == '11' & NDZ_sanemsanas_datums == "2022-11-04"))

    } else if (isTRUE(NDZ$period[1] == "_____") && kods == "50") {
      NDZ <- NDZ %>%
        mutate(
          zinkod = ifelse(
            PS_code == '__________' &
              NM_code == '__________' &
              zinkod == '50' &
              NDZ_sanemsanas_datums == '2022-11-01',
            '51',
            zinkod
          ),
          sak_beidz = ifelse(
            PS_code == '__________' &
              NM_code == '__________' &
              zinkod == '51' &
              NDZ_sanemsanas_datums == as.Date('2022-11-01'),
            '1',
            sak_beidz
          )
        )
      
    } else if (isTRUE(NDZ$period[1] == "_____" & NDZ$zinkod[1] == "40")) {
    NDZ <- NDZ %>% 
      dplyr::filter(!(PS_code == '__________' & NM_code == '_____' & zinkod == '25'))
  }
  return(NDZ) 
}

adj50 <- function(NDZ) {
    
 if (isTRUE(NDZ$period[1] ==  '______')) {
    
    
    NDZ <- NDZ %>% 
      dplyr::filter(!(PS_code  %in%  c'______________', '______________', '______________', '______________' & NM_code ==  '______________' & zinkod == '51'))
    
    NDZ <- NDZ %>% 
      dplyr::filter(!(PS_code  %in%  c'______________', '______________' & NM_code  ==  c'______________', '______________' & zinkod == '41'))

    NDZ <- NDZ %>% 
      dplyr::filter(!(PS_code  ==  '______________' & NM_code ==  '______________' & zinkod == '25'))
    
    NDZ <- NDZ %>% 
      dplyr::filter(!(PS_code  ==  '______________' & NM_code ==  '______________' & zinkod == '92'))  

 } else if (isTRUE(NDZ$period[1] ==  '______')) {
   NDZ <- NDZ %>% 
     dplyr::filter(PS_code  ==  '______________' & NM_code ==  '______________' & zinkod == '11') %>% 
     mutate(zinkod = '50')
   
 } else if (isTRUE(NDZ$period[1] ==  '______' & kods == "50")) {
   NDZ <- NDZ %>% 
     dplyr::filter(!(
       (PS_code  ==  '______________' & NM_code ==  '______________' & zinkod == '51') |
       (PS_code  ==  '______________' & NM_code ==  '______________' & zinkod == '51' & NDZ_sanemsanas_datums %in% c('2021-07-26', "2021-07-31")) 
      ))
   
 } else if (isTRUE(NDZ$period[1] ==  '______' & kods == "11")) {
   NDZ <- NDZ %>% 
     dplyr::filter(!(PS_code  ==  '______________' & NM_code ==  '______________' & zinkod == '11' & NDZ_sanemsanas_datums == '2021-08-02')) 

 } else if (isTRUE(NDZ$period[1] ==  '______' & kods == "50")) {
   NDZ <- NDZ %>% 
     dplyr::filter(!(
       (PS_code  ==  '______________' & NM_code ==  '______________' & zinkod == '51' & NDZ_sanemsanas_datums == '2021-09-26') | 
       (PS_code  ==  '______________' & NM_code ==  '______________' & zinkod == '50' & NDZ_sanemsanas_datums == '2021-09-27') 
     ))
 } else if (isTRUE(NDZ$period[1] ==  '______' & kods == "11")) {
   NDZ <- NDZ %>% 
     dplyr::filter(!(PS_code  ==  '______________' & NM_code ==  '______________' & NDZ_sanemsanas_datums == '2021-10-12'))

   NDZ <- NDZ %>% 
     dplyr::filter(!(PS_code  ==  '______________' & NM_code ==  '______________' & zinkod == '11' & NDZ_sanemsanas_datums == "2021-10-12"))
   
 } else if (isTRUE(NDZ$period[1] ==  '______' & kods == "11")) {
   NDZ <- NDZ %>% 
     dplyr::filter(!(PS_code  ==  '______________' & NM_code ==  '______________' & NDZ_sanemsanas_datums == '2021-11-02'))
   NDZ <- NDZ %>% 
     dplyr::filter(!(PS_code  ==  '______________' & NM_code ==  '______________' & zinkod == '11' & NDZ_sanemsanas_datums == "2021-11-02"))

 } else if (isTRUE(NDZ$period[1] ==  '______') && kods == "11") {
   NDZ <- NDZ %>% 
     dplyr::filter(PS_code  ==  '______________' & NM_code ==  '______________' & zinkod == '29') %>% 
     mutate(zinkod = '40')

   NDZ <- NDZ %>% 
     dplyr::filter(!(PS_code  ==  '______________' & NM_code ==  '______________' & NDZ_sanemsanas_datums == '2021-12-15'))

    } else if (isTRUE(NDZ$period[1] ==  '______' & kods == "50")) {
          NDZ <- NDZ %>% 
            dplyr::filter(!(PS_code  ==  '______________'& NM_code ==  '______________' & zinkod == '51' & NDZ_sanemsanas_datums == "2022-01-24"))
    
    
    } else if (isTRUE(NDZ$period[1] ==  '______' & kods == "40")) {
      
      NDZ <- NDZ %>% 
        dplyr::filter(!(
          (PS_code  ==  '______________'& NM_code ==  '______________' & zinkod == '40' & NDZ_sanemsanas_datums == "2022-02-16") | 
          (PS_code  ==  '______________'& NM_code ==  '______________' & zinkod == '91' & NDZ_sanemsanas_datums == "2022-02-01") |  
          (PS_code  ==  '______________'& NM_code ==  '______________' & zinkod == '91' & NDZ_sanemsanas_datums == "2022-02-04") |  
            #(PS_code  ==  '______________'& NM_code ==  '______________' & zinkod == '91' & NDZ_sanemsanas_datums == '2022-02-01') | 
          (PS_code  ==  '______________'& NM_code ==  '______________' & zinkod == '41' & NDZ_sanemsanas_datums == "2022-02-03")
        ))
      
      NDZ <- NDZ %>% 
        mutate(
          NDZ_sanemsanas_datums = if_else(
            PS_code  ==  '______________' &
              NM_code ==  '______________' &
              zinkod == '91' &
              NDZ_sanemsanas_datums == as.Date('2022-02-01'),
            as.Date('2022-02-02'),
            NDZ_sanemsanas_datums
          )
        )
    
    } else if (isTRUE(NDZ$period[1] ==  '______') && kods == "11") {
      NDZ <- NDZ %>% 
        dplyr::filter(!(
          (PS_code  ==  '______________'& NM_code ==  '______________' & zinkod == '11' & NDZ_sanemsanas_datums == "2022-02-21") |
          (PS_code  ==  '______________'& NM_code ==  '______________' & zinkod == '11' & NDZ_sanemsanas_datums == "2022-02-22") |
          (PS_code  ==  '______________'& NM_code ==  '______________' & zinkod == '25' & NDZ_sanemsanas_datums == "2022-02-01") |
          (PS_code  ==  '______________'& NM_code ==  '______________' & zinkod == '11' & NDZ_sanemsanas_datums == "2022-02-10") 
        ))
   
    } else if (isTRUE(NDZ$period[1] ==  '______' & kods == "11")) {
      NDZ <- NDZ %>%
        dplyr::filter(!(
          (PS_code  ==  '______________' & NM_code ==  '______________' & zinkod == '11' & NDZ_sanemsanas_datums == "2022-03-07") |
            (PS_code  ==  '______________' & NM_code ==  '______________' & zinkod == '11' & NDZ_sanemsanas_datums == "2022-03-12") |
            (PS_code  ==  '______________' & NM_code ==  '______________' & zinkod == '11' & NDZ_sanemsanas_datums == "2022-03-27") |
            (PS_code  ==  '______________' & NM_code ==  '______________' & zinkod == '11' & NDZ_sanemsanas_datums == "2022-03-29")
        ))
      
    } else if (isTRUE(NDZ$period[1] ==  '______' & kods == "40")) {
      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code  ==  '______________'& NM_code ==  '______________' & zinkod == '92' & NDZ_sanemsanas_datums == "2022-03-21"))

    } else if (isTRUE(NDZ$period[1] ==  '______' & kods == "50")) {
      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code  ==  '______________' & NM_code ==  '______________' & zinkod == '51' & NDZ_sanemsanas_datums == "2022-03-31"))
      
      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code  ==  '______________' & NM_code ==  '______________' & zinkod == '50' & NDZ_sanemsanas_datums == "2022-03-31"))
      

    } else if (isTRUE(NDZ$period[1] ==  '______' & kods == "11")) {
      NDZ <- NDZ %>% 
        dplyr::filter(!(
          (PS_code  ==  '______________' & NM_code ==  '______________' & zinkod == '11' & NDZ_sanemsanas_datums == "2022-04-05") |
          (PS_code  ==  '______________' & NM_code ==  '______________' & zinkod == '11' & NDZ_sanemsanas_datums == "2022-04-05")
        ))
      
    } else if (isTRUE(NDZ$period[1] ==  '______' & kods == "40")) {
      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code  ==  '______________'& NM_code ==  '______________' & zinkod == '91' & NDZ_sanemsanas_datums == "2022-04-21"))
      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code  ==  '______________'& NM_code ==  '______________' & zinkod == '91' & NDZ_sanemsanas_datums == "2022-04-21"))
      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code  ==  '______________'& NM_code ==  '______________' & zinkod == '92' & NDZ_sanemsanas_datums == "2022-04-30"))
      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code  ==  '______________'& NM_code ==  '______________' & zinkod == '92' & NDZ_sanemsanas_datums == "2022-04-30"))
      
    } else if (isTRUE(NDZ$period[1] ==  '______' & kods == "50")) {
      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code  ==  '______________'& NM_code ==  '______________' & zinkod == '51' & NDZ_sanemsanas_datums == "2022-04-30"))
      
      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code  ==  '______________'& NM_code ==  '______________' & zinkod == '50' & NDZ_sanemsanas_datums == "2022-04-16"))

      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code  ==  '______________'& NM_code ==  '______________' & zinkod == '51' & NDZ_sanemsanas_datums == "2022-04-01"))

      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code  ==  '______________'& NM_code ==  '______________' & zinkod == '51' & NDZ_sanemsanas_datums == "2022-04-28"))
      
      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code  ==  '______________'& NM_code ==  '______________' & zinkod == '51' & NDZ_sanemsanas_datums == "2022-04-20"))
      
    } else if (isTRUE(NDZ$period[1] ==  '______' & kods == "53")) {
      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code  ==  '______________'& NM_code ==  '______________' & zinkod == '53' & NDZ_sanemsanas_datums == "2022-04-30"))
  
    } else if (isTRUE(NDZ$period[1] ==  '______') && kods == "50") {
      
      NDZ <- NDZ %>% 
        mutate(
          NDZ_sanemsanas_datums = if_else(
            PS_code  ==  '______________' &
              NM_code ==  '______________' &
              zinkod == '50' &
              NDZ_sanemsanas_datums == as.Date('2022-05-01'),
            as.Date('2022-05-04'),
            NDZ_sanemsanas_datums
          )
        )
      
      
      NDZ <- NDZ %>% 
        mutate(
          zinkod = ifelse(
            PS_code  ==  '______________' &
              NM_code ==  '______________' &
              zinkod == '51' &
              NDZ_sanemsanas_datums == as.Date('2022-05-23'),
            '50',
            zinkod
          ),
          sak_beidz = ifelse(
            PS_code  ==  '______________' &
              NM_code ==  '______________' &
              zinkod == '50' &
              NDZ_sanemsanas_datums == as.Date('2022-05-23'),
            '2',
            sak_beidz
          )
        )
      
      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code  ==  '______________'& NM_code ==  '______________' & zinkod == '51' & NDZ_sanemsanas_datums == "2022-05-04"))
      
      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code  ==  '______________'& NM_code ==  '______________' & zinkod == '51' & NDZ_sanemsanas_datums == "2022-05-10"))
      

    } else if (isTRUE(NDZ$period[1] ==  '______' & kods == "53")) {
      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code  ==  '______________'& NM_code ==  '______________' & zinkod == '53' & NDZ_sanemsanas_datums == "2022-05-05"))
      
    } else if (isTRUE(NDZ$period[1] ==  '______' & kods == "11")) {
      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code  ==  '______________' & NM_code ==  '______________' & zinkod == '11' & NDZ_sanemsanas_datums == "2022-06-01"))
      
      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code  ==  '______________' & NM_code ==  '______________' & zinkod == '11' & NDZ_sanemsanas_datums == "2022-06-28"))

      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code  ==  '______________' & NM_code ==  '______________' & zinkod == '11' & NDZ_sanemsanas_datums == "2022-06-23"))

      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code  ==  '______________' & NM_code ==  '______________' & zinkod == '11' & NDZ_sanemsanas_datums == "2022-06-01"))
      
      
    } else if (isTRUE(NDZ$period[1] ==  '______' & kods == "50")) {
      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code  ==  '______________'& NM_code ==  '______________' & zinkod == '51' & NDZ_sanemsanas_datums == "2022-06-01"))

    } else if (isTRUE(NDZ$period[1] ==  '______' & kods == "40")) {
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
        dplyr::filter(!(PS_code  ==  '______________' & NM_code ==  '______________' & zinkod %in% c('41', '92') & NDZ_sanemsanas_datums == "2022-06-01"))
      
    } else if (isTRUE(NDZ$period[1] ==  '______' & kods == "53")) {
      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code  ==  '______________' & NM_code ==  '______________' & zinkod == '54' & NDZ_sanemsanas_datums == "2022-06-04"))
      
      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code  ==  '______________' & NM_code ==  '______________' & zinkod == '54' & NDZ_sanemsanas_datums == "2022-06-01"))
      
      
    } else if (isTRUE(NDZ$period[1] ==  '______' & kods == "11")) {
      NDZ <- NDZ %>%
        mutate(
          NDZ_sanemsanas_datums = if_else(
            PS_code  ==  '______________' &
              NM_code ==  '______________' &
              zinkod == '11' &
              NDZ_sanemsanas_datums == as.Date('2022-07-23'), 
            as.Date('2022-07-24'), 
            NDZ_sanemsanas_datums))
      
      NDZ <- NDZ %>% 
        dplyr::filter(!(
          (PS_code  ==  "______________" & NM_code  ==  "______________" & zinkod == "25" & NDZ_sanemsanas_datums == "2022-07-12") |
          (PS_code  ==  "______________" & NM_code  ==  "______________" & zinkod == "21" & NDZ_sanemsanas_datums == "2022-07-18") |
          (PS_code  ==  '______________' & NM_code ==  '______________' & zinkod == '11' & NDZ_sanemsanas_datums == "2022-07-15") |
          (PS_code  ==  '______________' & NM_code ==  '______________' & zinkod == '11' & NDZ_sanemsanas_datums == "2022-07-01")           
        ))

      
    } else if (isTRUE(NDZ$period[1] ==  '______' & kods == "40")) {
      NDZ <- NDZ %>%
        mutate(
          zinkod = ifelse(
            PS_code  ==  '______________' &
            NM_code ==  '______________' &
            zinkod == '51' &
            NDZ_sanemsanas_datums == as.Date('2022-07-01'), 
          '41', 
          zinkod))
      
      NDZ <- NDZ %>% 
        dplyr::filter(!(
          (PS_code  ==  '______________' & NM_code ==  '______________' & zinkod == '41' & NDZ_sanemsanas_datums == "2022-07-01") |
          (PS_code  ==  '______________' & NM_code ==  '______________' & zinkod == '92' & NDZ_sanemsanas_datums == "2022-07-14") |
          (PS_code  ==  '______________' & NM_code ==  '______________' & zinkod == '92' & NDZ_sanemsanas_datums == "2022-07-16") |
          (PS_code  ==  '______________' & NM_code ==  '______________' & zinkod == '40' & NDZ_sanemsanas_datums == "2022-07-04") |
          (PS_code  ==  '______________' & NM_code ==  '______________' & zinkod == '91' & NDZ_sanemsanas_datums == "2022-07-02") |
          (PS_code  ==  '______________' & NM_code ==  '______________' & zinkod == '41' & NDZ_sanemsanas_datums == "2022-07-11") |
            # šie trīs vienādie - fantastiska miskaste tur.
          (PS_code  ==  '______________' & NM_code ==  '______________' & zinkod == '92' & NDZ_sanemsanas_datums == "2022-07-12") | 
          (PS_code  ==  '______________' & NM_code ==  '______________' & zinkod == '92' & NDZ_sanemsanas_datums == "2022-07-29") |
          (PS_code  ==  '______________' & NM_code ==  '______________' & zinkod == '92' & NDZ_sanemsanas_datums == "2022-07-30") | 
          (PS_code  ==  '______________' & NM_code ==  '______________' & zinkod == '41' & NDZ_sanemsanas_datums == "2022-07-09") | 
          (PS_code  ==  '______________' & NM_code ==  '______________' & zinkod == '41' & NDZ_sanemsanas_datums == "2022-07-29")   
        ))
      

            
    
    } else if (isTRUE(NDZ$period[1] ==  '______' & kods == "50")) {
      NDZ <- NDZ %>% 
        dplyr::filter(!(
          (PS_code  ==  "______________" & NM_code  ==  "______________" & zinkod == "51" & NDZ_sanemsanas_datums == "2022-07-01") |
          (PS_code  ==  "______________" & NM_code  ==  "______________" & zinkod == "51" & NDZ_sanemsanas_datums == "2022-07-01") |
          (PS_code  ==  '______________' & NM_code ==  '______________' & zinkod == '51' & NDZ_sanemsanas_datums == "2022-07-01") |
          (PS_code  ==  '______________' & NM_code ==  '______________' & zinkod == '51' & NDZ_sanemsanas_datums == "2022-07-15") |  
          (PS_code  ==  '______________' & NM_code ==  '______________' & zinkod == '50' & NDZ_sanemsanas_datums == "2022-07-04"))) 
     
    } else if (isTRUE(NDZ$period[1] ==  '______' & kods == "11")) {
      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code  ==  '______________' & NM_code ==  '______________' & zinkod == '11' & NDZ_sanemsanas_datums == "2022-08-27"))
      
    } else if (isTRUE(NDZ$period[1] ==  '______' & kods == "40")) {

      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code  ==  '______________' & NM_code ==  '______________' & zinkod == '92' & NDZ_sanemsanas_datums == "2022-08-01"))
    
    } else if (isTRUE(NDZ$period[1] ==  '______' & kods == "50")) {
      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code  ==  '______________' & NM_code ==  '______________' & zinkod == '51' & NDZ_sanemsanas_datums == "2022-08-01"))
      
        
    } else if (isTRUE(NDZ$period[1] ==  '______') && kods == "11") {
      
      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code  ==  '______________'& NM_code ==  '______________' & zinkod == '11' & NDZ_sanemsanas_datums == "2022-11-04"))

    } else if (isTRUE(NDZ$period[1] ==  '______') && kods == "50") {
      NDZ <- NDZ %>%
        mutate(
          zinkod = ifelse(
            PS_code  ==  '______________' &
              NM_code ==  '______________' &
              zinkod == '50' &
              NDZ_sanemsanas_datums == '2022-11-01',
            '51',
            zinkod
          ),
          sak_beidz = ifelse(
            PS_code  ==  '______________' &
              NM_code ==  '______________' &
              zinkod == '51' &
              NDZ_sanemsanas_datums == as.Date('2022-11-01'),
            '1',
            sak_beidz
          )
        )
        

      
      
    } else if (isTRUE(NDZ$period[1] ==  '______')) {
            if (kods == "11") {
                    NDZ <- NDZ %>% 
                           dplyr::filter(!(PS_code  ==  '______________' & NM_code ==  '______________' & NDZ_sanemsanas_datums == '2023-02-15' & zinkod == '25'))
            } else if (kods == '50') {
                 NDZ <- NDZ %>% 
                          dplyr::filter(!(PS_code  ==  '______________' & NM_code ==  '______________' & zinkod == '51' & NDZ_sanemsanas_datums == '2023-02-18'))
            } 
  } else if (isTRUE(NDZ$period[1] ==  '______') & kods == "11") {
      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code  ==  '______________' & NM_code ==  '______________' & NDZ_sanemsanas_datums == '2023-07-28' & zinkod == '25'))
    }
  return(NDZ) 
}

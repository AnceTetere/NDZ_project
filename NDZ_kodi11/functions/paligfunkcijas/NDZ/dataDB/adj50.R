adj50 <- function(NDZ, kods) {
    
  if (isTRUE(NDZ$period[1] == '______') && kods == "50") {
    NDZ <- NDZ %>% 
      dplyr::filter(!(
        (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '50' & NDZ_sanemsanas_datums == as.Date('2021-01-04')) |- tas maldina
        (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '50' & NDZ_sanemsanas_datums == as.Date('2021-01-04')) | - tas maldina
        (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '50' & NDZ_sanemsanas_datums == as.Date('2021-01-18'))   #Kods maldina, jo indivīds aiziet bezalgas atvaļinājumā augustā.
      ))
    
  } else if (isTRUE(NDZ$period[1] == '______')) {
    NDZ <- NDZ %>% 
      dplyr::filter(!(PS_code  %in%  c'______________', '______________', '______________', '______________' & NM_code ==  '______________' & zinkod == '51'))
    
    NDZ <- NDZ %>% 
      dplyr::filter(!(PS_code  %in%  c'______________',  '______________' & NM_code  %in%  c'______________',  '______________' & zinkod == '41'))

    NDZ <- NDZ %>% 
      dplyr::filter(!(PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '25'))
    
    NDZ <- NDZ %>% 
      dplyr::filter(!(PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '92'))  
    
 } else if (isTRUE(NDZ$period[1] == '______') && kods == "40") {
   
    NDZ <- NDZ %>% 
       dplyr::filter(!(
         (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '40' & NDZ_sanemsanas_datums == '2021-03-15') 
     ))
    
 } else if (isTRUE(NDZ$period[1] == '______') && kods == "50") {
   
   NDZ <- NDZ %>% 
     dplyr::filter(!(
       (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '51' & NDZ_sanemsanas_datums %in% c('2021-04-01', '2021-04-19', '2021-04-20'))
     ))
   
 } else if (isTRUE(NDZ$period[1] == '______') && kods == "50") {
   
   NDZ <- NDZ %>% 
     dplyr::filter(!(
       (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '50' & NDZ_sanemsanas_datums == '2021-05-12')
     ))
   
 } else if (isTRUE(NDZ$period[1] == '______')) {
   NDZ <- NDZ %>% 
     dplyr::filter(PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '11') %>% 
     mutate(zinkod = '50')
   
 } else if (isTRUE(NDZ$period[1] == '______' & kods == "50")) {
   NDZ <- NDZ %>% 
     dplyr::filter(!(
       (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '51') |
       (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '51' & NDZ_sanemsanas_datums %in% c('2021-07-26', "2021-07-31")| 
       (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '51' & NDZ_sanemsanas_datums == "2021-07-12") | 
       (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '51' & NDZ_sanemsanas_datums == "2021-07-12"))  
      ))
   
 } else if (NDZ$period[1] == '______') {
          NDZ <- changes_202108(NDZ, kods)
   
 } else if (isTRUE(NDZ$period[1] == '______' & kods == "50")) {
   NDZ <- NDZ %>% 
     dplyr::filter(!(
       (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '51' & NDZ_sanemsanas_datums == '2021-09-26') | 
       (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '50' & NDZ_sanemsanas_datums == '2021-09-27') 
     ))
   
 } else if (isTRUE(NDZ$period[1] == '______' & kods == "11")) {
   NDZ <- NDZ %>% 
     dplyr::filter(!(
       (PS_code ==  '______________' & NM_code ==  '______________' & NDZ_sanemsanas_datums == '2021-10-12') |
       (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '11' & NDZ_sanemsanas_datums == "2021-10-12") |
       (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '11' & NDZ_sanemsanas_datums == "2021-10-07") | 
       (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '11' & NDZ_sanemsanas_datums == "2021-10-07") 
     ))
 
 } else if (isTRUE(NDZ$period[1] == '______' & kods == "50")) {
   
   NDZ <- NDZ %>% 
     dplyr::filter(!(
       (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '51' & NDZ_sanemsanas_datums == '2021-10-01') | 
       (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '51' & NDZ_sanemsanas_datums == '2021-10-01')   
         )) 
   
 } else if (NDZ$period[1] == '______') {
    NDZ <- changes_202111(NDZ, kods) 


 } else if (NDZ$period[1] == '______') {
         NDZ <- changes_202112(NDZ, kods)
   
 
 } else if (isTRUE(NDZ$period[1] == '______' & kods == "11")) {

      NDZ <- NDZ %>% 
         dplyr::filter(!(PS_code ==  '______________'& NM_code ==  '______________' & zinkod == '11' & NDZ_sanemsanas_datums == "2022-01-01")) 
   
 } else if (isTRUE(NDZ$period[1] == '______' & kods == "40")) {       
       NDZ <- NDZ %>% 
            dplyr::filter(!(
              (PS_code ==  '______________'& NM_code ==  '______________' & zinkod == '92' & NDZ_sanemsanas_datums == "2022-01-31") |
              (PS_code ==  '______________'& NM_code ==  '______________' & zinkod == '92' & NDZ_sanemsanas_datums == "2022-01-31")
            ))
   
       NDZ <- NDZ %>% 
         mutate(
           zinkod = if_else(
             PS_code ==  '______________' &
               NM_code ==  '______________' &
               zinkod == '41' &
               NDZ_sanemsanas_datums == as.Date('2022-01-21'),
             '40',
             zinkod
           ),
           sak_beidz = ifelse(
             PS_code ==  '______________' &
               NM_code ==  '______________' &
               zinkod == '40' &
               NDZ_sanemsanas_datums == as.Date('2022-01-21'),
             '2',
             sak_beidz
           )
         )
       
 } else if (isTRUE(NDZ$period[1] == '______' & kods == "50")) {
        
          NDZ <- NDZ %>% 
            dplyr::filter(!(
              (PS_code ==  '______________'& NM_code ==  '______________' & zinkod == '51' & NDZ_sanemsanas_datums == "2022-01-24") | 
              (PS_code ==  '______________'& NM_code ==  '______________' & zinkod == '51' & NDZ_sanemsanas_datums %in% c("2022-01-17", "2022-01-27")) |
              (PS_code ==  '______________'& NM_code ==  '______________' & zinkod == '50' & NDZ_sanemsanas_datums == "2022-01-03")    
          ))
    
    
    } else if (isTRUE(NDZ$period[1] == '______' & kods == "40")) {
      
      NDZ <- NDZ %>% 
        dplyr::filter(!(
          (PS_code ==  '______________'& NM_code ==  '______________' & zinkod == '40' & NDZ_sanemsanas_datums == "2022-02-16") | 
          (PS_code ==  '______________'& NM_code ==  '______________' & zinkod == '91' & NDZ_sanemsanas_datums == "2022-02-01") |  
          (PS_code ==  '______________'& NM_code ==  '______________' & zinkod == '91' & NDZ_sanemsanas_datums == "2022-02-04") |  
            #(PS_code ==  '______________'& NM_code ==  '______________' & zinkod == '91' & NDZ_sanemsanas_datums == '2022-02-01') | 
          (PS_code ==  '______________'& NM_code ==  '______________' & zinkod == '41' & NDZ_sanemsanas_datums == "2022-02-03")    
        ))
      
      NDZ <- NDZ %>% 
        mutate(
          NDZ_sanemsanas_datums = if_else(
            PS_code ==  '______________' &
              NM_code ==  '______________' &
              zinkod == '91' &
              NDZ_sanemsanas_datums == as.Date('2022-02-01'),
            as.Date('2022-02-02'),
            NDZ_sanemsanas_datums
          )
        )
    
    } else if (isTRUE(NDZ$period[1] == '______') && kods == "11") {
      NDZ <- NDZ %>% 
        dplyr::filter(!(
          (PS_code ==  '______________'& NM_code ==  '______________' & zinkod == '11' & NDZ_sanemsanas_datums == "2022-02-21") |
          (PS_code ==  '______________'& NM_code ==  '______________' & zinkod == '11' & NDZ_sanemsanas_datums == "2022-02-22") |
          (PS_code ==  '______________'& NM_code ==  '______________' & zinkod == '25' & NDZ_sanemsanas_datums == "2022-02-01") |
          (PS_code ==  '______________'& NM_code ==  '______________' & zinkod == '11' & NDZ_sanemsanas_datums == "2022-02-10") |
          (PS_code ==  '______________'& NM_code ==  '______________' & zinkod == '11' & NDZ_sanemsanas_datums == "2022-02-25") 
        ))
   
    } else if (isTRUE(NDZ$period[1] == '______' & kods == "11")) {
      NDZ <- NDZ %>%
        dplyr::filter(!(
          (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '11' & NDZ_sanemsanas_datums == "2022-03-07") |
            (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '11' & NDZ_sanemsanas_datums == "2022-03-12") |
            (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '11' & NDZ_sanemsanas_datums == "2022-03-27") |
            (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '11' & NDZ_sanemsanas_datums == "2022-03-29")
        ))
      
    } else if (isTRUE(NDZ$period[1] == '______' & kods == "40")) {
         
      NDZ <- NDZ %>% 
        dplyr::filter(!(
          (PS_code ==  '______________'& NM_code ==  '______________' & zinkod == '92' & NDZ_sanemsanas_datums == "2022-03-21") | 
          (PS_code ==  '______________'& NM_code ==  '______________' & zinkod == '92' & NDZ_sanemsanas_datums == "2022-03-12") | 
          (PS_code ==  '______________'& NM_code ==  '______________' & zinkod == '40' & NDZ_sanemsanas_datums == "2022-03-12")  
          ))

    } else if (isTRUE(NDZ$period[1] == '______' & kods == "50")) {
      
      NDZ <- NDZ %>% 
        dplyr::filter(!(
          (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '51' & NDZ_sanemsanas_datums == "2022-03-31") |
          (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '51' & NDZ_sanemsanas_datums == "2022-03-28") | 
          (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '50' & NDZ_sanemsanas_datums == "2022-03-31")   
          (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '51' & NDZ_sanemsanas_datums == "2022-03-31")   
        ))
      
    } else if (isTRUE(NDZ$period[1] == '______' & kods == "11")) {
      NDZ <- NDZ %>% 
        dplyr::filter(!(
          (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '11' & NDZ_sanemsanas_datums == "2022-04-05") |
          (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '11' & NDZ_sanemsanas_datums == "2022-04-05") |
          (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '11' & NDZ_sanemsanas_datums == "2022-04-21") | 
          (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '11' & NDZ_sanemsanas_datums == "2022-04-26")   
        ))
      
      
    } else if (isTRUE(NDZ$period[1] == '______' & kods == "40")) {
      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code ==  '______________'& NM_code ==  '______________' & zinkod == '91' & NDZ_sanemsanas_datums == "2022-04-21"))
      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code ==  '______________'& NM_code ==  '______________' & zinkod == '91' & NDZ_sanemsanas_datums == "2022-04-21"))
      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code ==  '______________'& NM_code ==  '______________' & zinkod == '92' & NDZ_sanemsanas_datums == "2022-04-30"))
      
      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code ==  '______________'& NM_code ==  '______________' & zinkod == '92' & NDZ_sanemsanas_datums == "2022-04-30"))
      
      NDZ <- NDZ %>% 
        mutate (
          zinkod = if_else(
            PS_code ==  '______________' &
              NM_code ==  '______________' &
              zinkod == '41' &
              NDZ_sanemsanas_datums == as.Date('2022-04-24'),
            '92',
            zinkod
          ))
      
      
    } else if (isTRUE(NDZ$period[1] == '______' & kods == "50")) {
      
      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code ==  '______________'& NM_code ==  '______________' & zinkod == '51' & NDZ_sanemsanas_datums == "2022-04-30"))
      
      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code ==  '______________'& NM_code ==  '______________' & zinkod == '50' & NDZ_sanemsanas_datums == "2022-04-16"))

      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code ==  '______________'& NM_code ==  '______________' & zinkod == '51' & NDZ_sanemsanas_datums == "2022-04-01"))

      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code ==  '______________'& NM_code ==  '______________' & zinkod == '51' & NDZ_sanemsanas_datums == "2022-04-28"))
      
      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code ==  '______________'& NM_code ==  '______________' & zinkod == '51' & NDZ_sanemsanas_datums == "2022-04-20"))
      
    } else if (isTRUE(NDZ$period[1] == '______' & kods == "53")) {
      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code ==  '______________'& NM_code ==  '______________' & zinkod == '53' & NDZ_sanemsanas_datums == "2022-04-30"))
  
    } else if (isTRUE(NDZ$period[1] == '______') && kods == "11") {
      NDZ <- NDZ %>% 
        dplyr::filter(!(
          (PS_code ==  '______________'& NM_code ==  '______________' & zinkod == '11' & NDZ_sanemsanas_datums == "2022-05-20") | 
          (PS_code ==  '______________'& NM_code ==  '______________' & zinkod == '11' & NDZ_sanemsanas_datums == "2022-05-29") |
          (PS_code ==  '______________'& NM_code ==  '______________' & zinkod == '11' & NDZ_sanemsanas_datums == "2022-05-23") |
          (PS_code ==  '______________'& NM_code ==  '______________' & zinkod == '11' & NDZ_sanemsanas_datums == "2022-05-21") 
        ))
      
    } else if (isTRUE(NDZ$period[1] == '______') && kods == "50") {
      
      
      NDZ <- NDZ %>% 
        mutate(
          NDZ_sanemsanas_datums = if_else(
            PS_code ==  '______________' &
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
            PS_code ==  '______________' &
              NM_code ==  '______________' &
              zinkod == '51' &
              NDZ_sanemsanas_datums == as.Date('2022-05-23'),
            '50',
            zinkod
          ),
          sak_beidz = ifelse(
            PS_code ==  '______________' &
              NM_code ==  '______________' &
              zinkod == '50' &
              NDZ_sanemsanas_datums == as.Date('2022-05-23'),
            '2',
            sak_beidz
          )
        )
      
      NDZ <- NDZ %>% 
        dplyr::filter(!(
          (PS_code ==  '______________'& NM_code ==  '______________' & zinkod == '51' & NDZ_sanemsanas_datums == "2022-05-04") | 
          (PS_code ==  '______________'& NM_code ==  '______________' & zinkod == '51' & NDZ_sanemsanas_datums == "2022-05-10") | 
          (PS_code ==  '______________'& NM_code ==  '______________' & zinkod == '51' & NDZ_sanemsanas_datums == "2022-05-26") | 
          (PS_code ==  '______________'& NM_code ==  '______________' & zinkod == '51' & NDZ_sanemsanas_datums == "2022-05-12")  
        ))
      

    } else if (isTRUE(NDZ$period[1] == '______' & kods == "53")) {
      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code ==  '______________'& NM_code ==  '______________' & zinkod == '53' & NDZ_sanemsanas_datums == "2022-05-05"))
      
    } else if (NDZ$period[1] == '______') {
            NDZ <- changes_202206(NDZ, kods)
            
    } else if (isTRUE(NDZ$period[1] == '______' & kods == "11")) {
      NDZ <- NDZ %>%
        mutate(
          NDZ_sanemsanas_datums = if_else(
            PS_code ==  '______________' &
              NM_code ==  '______________' &
              zinkod == '11' &
              NDZ_sanemsanas_datums == as.Date('2022-07-23'), 
            as.Date('2022-07-24'), 
            NDZ_sanemsanas_datums))
      
      NDZ <- NDZ %>% 
        dplyr::filter(!(
          (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == "25" & NDZ_sanemsanas_datums == "2022-07-12") |
          (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == "21" & NDZ_sanemsanas_datums == "2022-07-18") |
          (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '11' & NDZ_sanemsanas_datums == "2022-07-15") |
          (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '11' & NDZ_sanemsanas_datums == "2022-07-01") |
          (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '11' & NDZ_sanemsanas_datums == "2022-07-28") 
          
        ))

      
    } else if (isTRUE(NDZ$period[1] == '______' & kods == "40")) {
      
      NDZ <- NDZ %>%
        mutate(
          zinkod = ifelse(
            PS_code ==  '______________' &
            NM_code ==  '______________' &
            zinkod == '51' &
            NDZ_sanemsanas_datums == as.Date('2022-07-01'), 
          '41', 
          zinkod))
      
      
      
      NDZ <- NDZ %>% 
        dplyr::filter(!(
          (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '41' & NDZ_sanemsanas_datums == "2022-07-01") |
          (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '92' & NDZ_sanemsanas_datums == "2022-07-14") |
          (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '92' & NDZ_sanemsanas_datums == "2022-07-16") |
          (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '40' & NDZ_sanemsanas_datums == "2022-07-04") |
          (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '91' & NDZ_sanemsanas_datums == "2022-07-02") |
          (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '41' & NDZ_sanemsanas_datums == "2022-07-11") |
            
          (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '92' & NDZ_sanemsanas_datums == "2022-07-12") | 
          (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '92' & NDZ_sanemsanas_datums == "2022-07-29") |
          (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '92' & NDZ_sanemsanas_datums == "2022-07-30") | 
          (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '41' & NDZ_sanemsanas_datums == "2022-07-09") |  
          (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '41' & NDZ_sanemsanas_datums == "2022-07-29") |  
          (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '41' & NDZ_sanemsanas_datums == "2022-07-15")    
        ))
      
    } else if (isTRUE(NDZ$period[1] == '______' & kods == "50")) {
      NDZ <- NDZ %>% 
        dplyr::filter(!(
          (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == "51" & NDZ_sanemsanas_datums == "2022-07-01") |
          (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == "51" & NDZ_sanemsanas_datums == "2022-07-01") |
          (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '51' & NDZ_sanemsanas_datums == "2022-07-01") |
          (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '51' & NDZ_sanemsanas_datums == "2022-07-15") |  
          (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '50' & NDZ_sanemsanas_datums == "2022-07-04") |  
          (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '51' & NDZ_sanemsanas_datums == "2022-07-25") |  
          (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '51' & NDZ_sanemsanas_datums == "2022-07-20")    
     ))
    } else if (NDZ$period[1] == '______') {
          NDZ <- changes_202208(NDZ, kods)               
        
    } else if (NDZ$period[1] == '______') {
          NDZ <- changes_202209(NDZ, kods) 
    
    } else if (NDZ$period[1] == '______') {
             NDZ <- changes_202210(NDZ, kods)
    } else if (isTRUE(NDZ$period[1] == '______') && kods == "11") {
      
      
      
      
      
      
      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code ==  '______________'& NM_code ==  '______________' & zinkod == '11' & NDZ_sanemsanas_datums == "2022-11-04"))

    } else if (isTRUE(NDZ$period[1] == '______') && kods == "50") {
      NDZ <- NDZ %>%
        mutate(
          zinkod = ifelse(
            PS_code ==  '______________' &
              NM_code ==  '______________' &
              zinkod == '50' &
              NDZ_sanemsanas_datums == '2022-11-01',
            '51',
            zinkod
          ),
          sak_beidz = ifelse(
            PS_code ==  '______________' &
              NM_code ==  '______________' &
              zinkod == '51' &
              NDZ_sanemsanas_datums == as.Date('2022-11-01'),
            '1',
            sak_beidz
          )
        )
        
    } else if (isTRUE(NDZ$period[1] == '______') && kods == "11") {
      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code ==  '______________' & NM_code ==  '______________' & NDZ_sanemsanas_datums == '2022-12-22' & zinkod == '25')) 
      
    } else if (isTRUE(NDZ$period[1] == '______') && kods == "11") {
               NDZ <- NDZ %>% 
                        dplyr::filter(!(PS_code ==  '______________' & NM_code ==  '______________' & NDZ_sanemsanas_datums == '2023-01-31' & zinkod == '25')) 
      
    } else if (isTRUE(NDZ$period[1] == '______')) {
      NDZ <- changes_202302(NDZ, kods)      

    } else if (isTRUE(NDZ$period[1] == '______') & kods == "50") {
      NDZ <- NDZ %>% 
        dplyr::filter(!(
                 (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == "51" & NDZ_sanemsanas_datums == "2022-03-25") 
        ))
      
  } else if (isTRUE(NDZ$period[1] == '______') & kods == "11") {
      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code ==  '______________' & NM_code ==  '______________' & NDZ_sanemsanas_datums == '2023-07-28' & zinkod == '25'))
  
  } else if (isTRUE(NDZ$period[1] == '______') & kods == "50") {
    NDZ <- NDZ %>% 
      dplyr::filter(!(PS_code ==  '______________' & NM_code ==  '______________' & NDZ_sanemsanas_datums == '2023-07-04' & zinkod == '51')) 

  } else if (isTRUE(NDZ$period[1] == '______') & kods == "50") {
  
    NDZ <- NDZ %>%
      mutate(
        zinkod = ifelse(
          PS_code ==  '______________' &
            NM_code ==  '______________' &
            zinkod == '51' &
            NDZ_sanemsanas_datums == '2023-08-01',
          '50',
          zinkod
        ),
        sak_beidz = ifelse(
          PS_code ==  '______________' &
            NM_code ==  '______________' &
            zinkod == '50' &
            NDZ_sanemsanas_datums == as.Date('2023-08-01'),
          '2',
          sak_beidz
        )
      )
      
  } else if (isTRUE(NDZ$period[1] == '______') & kods == "50") {
    NDZ <- NDZ %>% 
      dplyr::filter(!(PS_code ==  '______________' & NM_code ==  '______________' & NDZ_sanemsanas_datums == '2023-09-20' & zinkod == '50')) 
    
  }
  return(NDZ) 
}



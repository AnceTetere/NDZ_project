adj50 <- function(NDZ) {
    
 if (isTRUE(NDZ$period[1] == "______")) {
    
    NDZ <- NDZ %>% 
      dplyr::filter(!(PS_code %in% c("___________", "___________", "___________", "___________") & NM_code == "______________" & zinkod == "51"))
    
    NDZ <- NDZ %>% 
      dplyr::filter(!(PS_code %in% c("___________", "___________") & NM_code == c("___________", "___________1") & zinkod == "41"))
    NDZ <- NDZ %>% 
      dplyr::filter(!(PS_code == "______________" & NM_code == "______________" & zinkod == "25"))
    
    NDZ <- NDZ %>% 
      dplyr::filter(!(PS_code == "______________" & NM_code == "______________" & zinkod == "92"))  

 } else if (isTRUE(NDZ$period[1] == "______")) {
   NDZ <- NDZ %>% 
     dplyr::filter(PS_code == "____________" & NM_code == "______________" & zinkod == "11") %>% 
     mutate(zinkod = "50")

 } else if (isTRUE(NDZ$period[1] == "______" & kods == "11")) {
   NDZ <- NDZ %>% 
     dplyr::filter(!(PS_code == "______________" & NM_code == "______________" & NDZ_sanemsanas_datums == "______________"))

 } else if (isTRUE(NDZ$period[1] == "______" & kods == "11")) {
   NDZ <- NDZ %>% 
     dplyr::filter(!(PS_code == "______________" & NM_code == "______________" & NDZ_sanemsanas_datums == "______________"))
   
 } else if (isTRUE(NDZ$period[1] == "______") && kods == "11") {
   NDZ <- NDZ %>% 
     dplyr::filter(PS_code == "______________" & NM_code == "______________" & zinkod == "29") %>% 
     mutate(zinkod = "40")
   
  } else if (isTRUE(NDZ$period[1] == "______" & kods == "50")) {
    NDZ <- NDZ %>% 
      dplyr::filter(!(PS_code == "______________" & NM_code == "______________" & zinkod == "51"))
    
  } else if (isTRUE(NDZ$period[1] == "______" & kods == "11")) {
    NDZ <- NDZ %>% 
      dplyr::filter(!(PS_code == "______________" & NM_code == "______________" & zinkod == "11" & NDZ_sanemsanas_datums == "______________"))

  } else if (isTRUE(NDZ$period[1] == "______" & kods == "11")) {
    NDZ <- NDZ %>% 
      dplyr::filter(!(PS_code == "______________" & NM_code == "______________" & zinkod == "11" & NDZ_sanemsanas_datums == "______________"))
    
    
    } else if (isTRUE(NDZ$period[1] == "______" & kods == "40")) {
      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code == "______________"& NM_code == "______________" & zinkod == "91" & NDZ_sanemsanas_datums == "______________"))
      
      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code == "______________"& NM_code == "______________" & zinkod == "40" & NDZ_sanemsanas_datums == "______________"))
      
      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code == "______________"& NM_code == "______________" & zinkod == "91" & NDZ_sanemsanas_datums == "______________"))
    
    } else if (isTRUE(NDZ$period[1] == "______") && kods == "11") {
      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code == "______________"& NM_code == "______________" & zinkod == "11" & NDZ_sanemsanas_datums == "______________"))
      
      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code == "______________"& NM_code == "______________" & zinkod == "11" & NDZ_sanemsanas_datums == "______________"))
      
      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code == "______________"& NM_code == "______________" & zinkod == "25" & NDZ_sanemsanas_datums == "______________"))
   
    } else if (isTRUE(NDZ$period[1] == "______" & kods == "11")) {
      NDZ <- NDZ %>%
        dplyr::filter(!(
          (PS_code == "______________" & NM_code == "______________" & zinkod == "11" & NDZ_sanemsanas_datums == "______________") |
            (PS_code == "______________" & NM_code == "______________" & zinkod == "11" & NDZ_sanemsanas_datums == "______________") |
            (PS_code == "______________" & NM_code == "______________" & zinkod == "11" & NDZ_sanemsanas_datums == "______________") |
            (PS_code == "______________" & NM_code == "______________" & zinkod == "11" & NDZ_sanemsanas_datums == "______________")
        ))
      
    } else if (isTRUE(NDZ$period[1] == "______" & kods == "40")) {
      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code == "______________"& NM_code == "______________" & zinkod == "92" & NDZ_sanemsanas_datums == "______________"))

    } else if (isTRUE(NDZ$period[1] == "______" & kods == "50")) {
      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code == "______________" && NM_code == "______________" & zinkod == "51" & NDZ_sanemsanas_datums == "______________"))

    } else if (isTRUE(NDZ$period[1] == "______" & kods == "11")) {
      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code == "______________"& NM_code == "______________" & zinkod == "11" & NDZ_sanemsanas_datums == "______________"))
      
    } else if (isTRUE(NDZ$period[1] == "______" & kods == "40")) {
      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code == "______________"& NM_code == "______________" & zinkod == "91" & NDZ_sanemsanas_datums == "______________"))
      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code == "______________"& NM_code == "______________" & zinkod == "91" & NDZ_sanemsanas_datums == "______________"))
      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code == "______________"& NM_code == "______________" & zinkod == "92" & NDZ_sanemsanas_datums == "______________"))
      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code == "______________"& NM_code == "______________" & zinkod == "92" & NDZ_sanemsanas_datums == "______________"))
      
    } else if (isTRUE(NDZ$period[1] == "______" & kods == "50")) {
      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code == "______________"& NM_code == "______________" & zinkod == "51" & NDZ_sanemsanas_datums == "______________"))
      
      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code == "______________"& NM_code == "______________" & zinkod == "50" & NDZ_sanemsanas_datums == "______________"))

      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code == "______________"& NM_code == "______________" & zinkod == "51" & NDZ_sanemsanas_datums == "______________"))

      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code == "______________"& NM_code == "______________" & zinkod == "51" & NDZ_sanemsanas_datums == "______________"))
      
    } else if (isTRUE(NDZ$period[1] == "______" & kods == "53")) {
      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code == "______________"& NM_code == "______________" & zinkod == "53" & NDZ_sanemsanas_datums == "______________"))
  
    } else if (isTRUE(NDZ$period[1] == "______") && kods == "50") {
            
      NDZ <- NDZ %>% 
        mutate(
          NDZ_sanemsanas_datums = if_else(
            PS_code == "______________" &
              NM_code == "______________" &
              zinkod == "50" &
              NDZ_sanemsanas_datums == as.Date("______________"),
            as.Date("______________"),
            NDZ_sanemsanas_datums
          )
        )
      
      
      NDZ <- NDZ %>% 
        mutate(
          zinkod = ifelse(
            PS_code == "______________" &
              NM_code == "______________" &
              zinkod == "51" &
              NDZ_sanemsanas_datums == as.Date("______________"),
            "50",
            zinkod
          ),
          sak_beidz = ifelse(
            PS_code == "______________" &
              NM_code == "______________" &
              zinkod == "50" &
              NDZ_sanemsanas_datums == as.Date("______________"),
            "2",
            sak_beidz
          )
        )
      
      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code == "______________"& NM_code == "______________" & zinkod == "51" & NDZ_sanemsanas_datums == "______________"))

    } else if (isTRUE(NDZ$period[1] == "______" & kods == "53")) {
      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code == "______________"& NM_code == "______________" & zinkod == "53" & NDZ_sanemsanas_datums == "______________"))
      
    } else if (isTRUE(NDZ$period[1] == "______" & kods == "50")) {
      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code == "______________"& NM_code == "______________" & zinkod == "51" & NDZ_sanemsanas_datums == "______________"))
      
    } else if (isTRUE(NDZ$period[1] == "______" & kods == "40")) {
      NDZ <- NDZ %>%
        mutate(
          zinkod = ifelse(
            PS_code == "______________" &
            NM_code == "______________" &
            zinkod == "51" &
            NDZ_sanemsanas_datums == as.Date("______________"), 
          "41", 
          zinkod))

    } else if (isTRUE(NDZ$period[1] == "______" & kods == "11")) {
      
      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code == "______________" & NM_code == "______________" & zinkod == "25" & NDZ_sanemsanas_datums == "______________"))
      
    } else if (isTRUE(NDZ$period[1] == "______") && kods == "11") {
      
      NDZ <- NDZ %>% 
        dplyr::filter(!(PS_code == "______________"& NM_code == "______________" & zinkod == "11" & NDZ_sanemsanas_datums == "______________"))

    } else if (isTRUE(NDZ$period[1] == "______") && kods == "50") {
      NDZ <- NDZ %>%
        mutate(
          zinkod = ifelse(
            PS_code == "______________" &
              NM_code == "______________" &
              zinkod == "50" &
              NDZ_sanemsanas_datums == "______________",
            "51",
            zinkod
          ),
          sak_beidz = ifelse(
            PS_code == "______________" &
              NM_code == "______________" &
              zinkod == "51" &
              NDZ_sanemsanas_datums == as.Date("______________"),
            "1",
            sak_beidz
          )
        )
        

      
      
    } else if (isTRUE(NDZ$period[1] == "______" & NDZ$zinkod[1] == "40")) {
    NDZ <- NDZ %>% 
      dplyr::filter(!(PS_code == "______________" & NM_code == "______________" & zinkod == "25"))
  }
  return(NDZ) 
}

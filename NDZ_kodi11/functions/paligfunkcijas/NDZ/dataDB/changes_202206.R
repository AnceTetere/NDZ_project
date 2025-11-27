 changes_202206 <- function(NDZ, kods) {
  
  if (kods == "11") {
             
             NDZ <- NDZ %>% 
               dplyr::filter(!(
                 (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '11' & NDZ_sanemsanas_datums == "2022-06-01") | #Te dzilāka izpēte rāda, ka cauri gadiem indivīds tiek pieņemts darbā uz vienu dienu (visdrīzāk pašnodarbinātais). Sekojošais kods noatbilst šai sistemātikai. Tur ir kļūda; pieņemu lēmumu to dzēst.
                 (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '11' & NDZ_sanemsanas_datums == "2022-06-28") | #Te nevar saprast. 202206 indivīds tiek pieņemts darbā, bet tad atkal pieņemts darbā 202302. Dzilāka izpēte liek domāt, ka 202206 darbā pieņemšanas kods varētu būt kļūda. Bet tā var arī nebūt kļūda.
                 (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '11' & NDZ_sanemsanas_datums == "2022-06-23") | #Te nevar saprast. 20220623 indivīds tiek atlaists un pieņemts darbā, bet tad atkal pieņemts darbā 20220712. Dzilāka izpēte liek domāt, ka 202206 darbā pieņemšanas kods varētu būt kļūda. Bet tā var arī nebūt kļūda.
                 (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '11' & NDZ_sanemsanas_datums == "2022-06-01") | #Dzilāka izpēte rāda, ka šis kods varētu būt kļūda, jo cilvēks jau ir nodarbinātībā un bieži ņem bezalgas atvaļinājumus, un pa reizei pie atgriešanās parādas šis 11 kods vienā datumā ar 51, lai arī nekāda atlaišana nav notikusi.
                 (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '11' & NDZ_sanemsanas_datums == "2022-06-24") |  
                 (PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '11' & NDZ_sanemsanas_datums == "2022-06-21")    #Šis kods maldina, jo darbs tiek sākts augustā
                ))
  
                
    
  } else if (kods == "40") {
              NDZ <- NDZ %>% 
                mutate(
                  zinkod = ifelse(
                    PS_code  %in%  c'______________', '______________') &
                      NM_code  %in%  c'______________', '______________') &
                      zinkod == '40' &
                      NDZ_sanemsanas_datums %in% c(as.Date('2022-06-28'), as.Date('2022-06-15')),
                    '41', 
                    zinkod),
                  sak_beidz = ifelse(
                    PS_code  %in%  c'______________', '______________') &
                      NM_code  %in%  c'______________', '______________') &
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
                 (PS_code ==  '______________'& NM_code ==  '______________' & zinkod == '51' & NDZ_sanemsanas_datums == "2022-06-01") | 
                 (PS_code ==  '______________'& NM_code ==  '______________' & zinkod == '50' & NDZ_sanemsanas_datums == "2022-06-01")  
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
                 (PS_code ==  '______________'& NM_code ==  '______________' & zinkod == '51' & NDZ_sanemsanas_datums == "2022-06-01")  
               ))
    
  } else if (kods == "53") {
              NDZ <- NDZ %>% 
                dplyr::filter(!(PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '54' & NDZ_sanemsanas_datums == "2022-06-04"))
              
              NDZ <- NDZ %>% 
                dplyr::filter(!(PS_code ==  '______________' & NM_code ==  '______________' & zinkod == '54' & NDZ_sanemsanas_datums == "2022-06-01"))
  }
  
  return(NDZ)
}

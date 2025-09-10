processingFourteen <- function(x, o, kods) {
  x <- x %>% arrange(PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
  x14_uzVieniniekiem <- data.frame(); x14_uzDivi <- data.frame(); x14_uzDivpadsmit <- data.frame(); x14_uzTrispadsmit <- data.frame()
  
  for (r in seq(1, nrow(x), by = 14)) {
    
    x14 <- x[r:(r+13),] %>% arrange(PS_code, DN_code, NM_code, NDZ_sanemsanas_datums, zinkod)
    
    if (sum(x14$sak_beidz == "1") == 7) {
      if (all(x14$sak_beidz[c(2, 3, 5, 8, 9, 12, 14)] == "1") && 
          x14$NDZ_sanemsanas_datums[1] == x14$NDZ_sanemsanas_datums[2] &&
          x14$NDZ_sanemsanas_datums[2] != x14$NDZ_sanemsanas_datums[3]) {
        x14_uzDivi <- rbind(x14_uzDivi, x14[1:2, ])    
        x14_uzDivpadsmit <- rbind(x14_uzDivpadsmit, x14[-(1:2), ])    
      } else if (all(x14$sak_beidz[seq(1, 14, by = 2)] == "1") && 
                 x14$NDZ_sanemsanas_datums[1] <= x14$NDZ_sanemsanas_datums[2] &&
                 x14$NDZ_sanemsanas_datums[2] != x14$NDZ_sanemsanas_datums[3]) {
        x14_uzDivi <- rbind(x14_uzDivi, x14[1:2, ])    
        x14_uzDivpadsmit <- rbind(x14_uzDivpadsmit, x14[-(1:2), ])    
      } else if (all(x14$sak_beidz[seq(2, 14, by = 2)] == "1") && 
                 all(diff(x14$NDZ_sanemsanas_datums) != 0)) {
        x14_uzVieniniekiem <- rbind(x14_uzVieniniekiem, x14[1, ])    
        x14_uzTrispadsmit <- rbind(x14_uzTrispadsmit, x14[-1, ])    
      } else if (all(x14$sak_beidz[1:4] == c("1", "2", "1", "2"))) { 
                 if (all(diff(x14$NDZ_sanemsanas_datums[1:4]) != 0)) {
                   #JO PIRMOREIZ
                   if (x14$period[1] == '______' && x14$PS_code[1] ==  '______________' && x14$NM_code[1] ==  '______________') {
                           x14_uzDivi <- rbind(x14_uzDivi, x14[1:2, ])    
                           x14_uzDivpadsmit <- rbind(x14_uzDivpadsmit, x14[-(1:2), ])
                           if (x14$sak_beidz[1] == "1" && kods %in% c("40", "50", "53") && o == "14") {ZERO_minus(x14 %>% slice(1))}
                   } else {stop("14-nieku tabulas pārdalei trūkst izstrādes koda. \n")}
                 } else {stop("14-nieku tabulas pārdalei trūkst izstrādes koda. \n")}
              } else {stop("14-nieku tabulas pārdalei trūkst izstrādes koda. \n")}
    } else {stop("14-nieku tabulas pārdalei trūkst izstrādes koda. \n")} 

  }
  
  #PĀRBAUDE: Vai rindu skaits no 14-niekiem atvasinātajās tabulās sakrīt ar rindām izejas tabulā x.
  if (nrow(x14_uzVieniniekiem) + nrow(x14_uzDivi) + nrow(x14_uzDivpadsmit) + nrow(x14_uzTrispadsmit) == nrow(x)) {
    cat("PĀRBAUDE IZIETA: Apakštabulu rindu summa sakrīt ar izejošo 13-nieku tabulu.\n"); rm(x, x14, r)
  } else {stop("ERROR: Apakštabulu rindu summa NESAKRĪT ar izejošo 14-nieku tabulu.\n")}
  
  #1 Apakštabulu x14_uzVieniniekiem sūta caur processingOnes().
  if (nrow(x14_uzVieniniekiem) > 0) {
    x14_uzVieniniekiem %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums, zinkod) %>% processingOnes(o) %>% sendTo_tempNDZ(o)
  } else {cat("Tabula x14_uzVieniniekiem ir tukša.\n")}
  rm(x14_uzVieniniekiem)
  
  #2 Apakštabulu x14_uzDivi sūta caur processingTwoes().
  if(nrow(x14_uzDivi) > 0) {
    x14_uzDivi %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums, sak_beidz) %>% processingTwoes(o, kods)
  } else {cat("Tabula x14_uzDivi ir tukša.\n")}
  rm(x14_uzDivi)
  
  #5 Apakštabulu x14_uzDivpadsmit sūta caur processingTwelve().
  if (nrow(x14_uzDivpadsmit) > 0) {
    x14_uzDivpadsmit %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums, sak_beidz) %>% processingTwelve(o, kods)
  } else {cat("Tabula x14_uzDivpadsmit ir tukša.\n")}
  rm(x14_uzDivpadsmit)
  
  #6 Apakštabulu x14_uzTrispadsmit sūta caur processingThirteen().
  if (nrow(x14_uzTrispadsmit) > 0) {
    x14_uzTrispadsmit %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums, sak_beidz) %>% processingThirteen(o, kods)
  } else {cat("Tabula x14_uzTrispadsmit ir tukša.\n")}
  rm(x14_uzTrispadsmit)
}

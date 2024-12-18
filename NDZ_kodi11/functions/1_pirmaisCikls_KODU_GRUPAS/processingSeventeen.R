processingSeventeen <- function(x, o) {

  x <- x %>% arrange(PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
  x17_uzVieniniekiem <- data.frame(); x17_uzDivniekiem <- data.frame(); x17_uzTrīspadsmit <- data.frame(); x17_uz15 <- data.frame(); x17_uzSespadsmit <- data.frame() 

  for (r in seq(1, nrow(x), by = 17)) {
    x17 <- x[r:(r+16), ] %>% arrange(PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
    
if(sum(x17$sak_beidz == "2") == 9){
      if (all(x17$sak_beidz[1:2] == c("2", "1"))) {
        if (diff(x17$NDZ_sanemsanas_datums[1:2]) != 0) {
          x17_uzVieniniekiem <- rbind(x17_uzVieniniekiem, x17[1, ])
          x17_uzSespadsmit <- rbind(x17_uzSespadsmit, x17[-1, ])
        } else {stop("17-nieku tabulās trūkst izstrādes koda.")}
      } else {stop(cat("ERROR: 17-nieku tabulās trūkst izstrādes koda. Rindas: ", r, ":", r+16))}
} else if (sum(x17$sak_beidz == "1") == 9) {
      if (all(x17$sak_beidz[1:4] == c("1", "2", "1", "2"))) {
        if (all(sapply(c(2,4), function(i) diff(x17$NDZ_sanemsanas_datums[i:(i+1)] != 0)))) {
          x17_uzDivniekiem <- rbind(x17_uzDivniekiem, x17[1:4, ])
          x17_uzTrīspadsmit <- rbind(x17_uzTrīspadsmit, x17[-(1:4), ])
        } else if (all(diff(x17$NDZ_sanemsanas_datums) != 0)) {
          x17_uzDivniekiem <- rbind(x17_uzDivniekiem, x17[1:2, ])
          x17_uz15 <- rbind(x17_uz15, x17[-(1:2), ])
        } else {stop("17-nieku tabulās trūkst izstrādes koda.")}
      } else if (x17$sak_beidz[17] == "1") {
        if(diff(x17$NDZ_sanemsanas_datums[16:17] != 0)) {
          x17_uzVieniniekiem <- rbind(x17_uzVieniniekiem, x17[17, ])
          x17_uzSespadsmit <- rbind(x17_uzSespadsmit, x17[-17, ])
        } else {stop("17-nieku tabulās trūkst izstrādes koda.")}
      } else {stop("17-nieku tabulās trūkst izstrādes koda. Rindas: ", r, ":", r+16)}
    } else {stop("17-nieku tabulās trūkst izstrādes koda. Rindas: ", r, ":", r+16)}
  }
  
  #PĀRBAUDE: Vai rindu skaits no 17-niekiem atvasinātajās tabulās sakrīt ar rindām izejas tabulā x.
  if (sum(nrow(x17_uzVieniniekiem), nrow(x17_uzDivniekiem), nrow(x17_uzTrīspadsmit), nrow(x17_uz15), nrow(x17_uzSespadsmit)) == nrow(x)) {
    cat("PĀRBAUDE IZIETA: Apakštabulu rindu summa sakrīt ar izejošo 17-nieku tabulu.\n"); rm(x, x17, r)
  } else {stop("PĀRBAUDE NAV IZIETA: Apakštabulu rindu summa NESAKRĪT ar izejošo 17-nieku tabulu.")}
  
  #1 Apakštabulu x17_uzVieniniekiem sūta caur processingOnes().
  if (nrow(x17_uzVieniniekiem) > 0) {
    x17_uzVieniniekiem %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums, sak_beidz) %>% 
      processingOnes(o) %>% sendTo_tempNDZ(o)
  } else {cat("Tabula x17_uzVieniniekiem ir tukša.\n")}
  rm(x17_uzVieniniekiem)
  
  #2 Apakštabulu x17_uzDivniekiem sūta caur processingTwoes().
  if (nrow(x17_uzDivniekiem) > 0) {
    x17_uzDivniekiem %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingTwoes(o)
  } else {cat("Tabula x17_uzDivniekiem ir tukša.\n")}
  rm(x17_uzDivniekiem)
  
  #3 Apakštabulu x17_uzTrīspadsmit sūta caur processingThirteen().
  if (nrow(x17_uzTrīspadsmit) > 0) {
   x17_uzTrīspadsmit %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingThirteen(o)
  } else {cat("Tabula x17_uzTrīspadsmit ir tukša.\n")}
  rm(x17_uzTrīspadsmit)
  
  #4 Apakštabulu x17_uz15 sūta caur processingFifteen().
  if (nrow(x17_uz15) > 0) {
    x17_uz15 %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingFifteen(o)
  } else {cat("Tabula x17_uzTrīspadsmit ir tukša.\n")}
  rm(x17_uz15)
  
  #5 Apakštabulu x17_uzSespadsmit sūta caur processingSixteen().
  if (nrow(x17_uzSespadsmit) > 0) {
    x17_uzSespadsmit %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% 
      processingSixteen(o)
  } else {cat("Tabula x17_uzSespadsmit ir tukša.\n")}
  rm(x17_uzSespadsmit)
}

processingNines <- function(x, o, kods) {
  x <- arrange(x, PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
  x9_uzVieniniekiem <- data.frame(); x9_uzDivi <- data.frame();  x9_uzSesi <- data.frame(); x9_uzSeptini <- data.frame(); x9_uzAstoniekiem <- data.frame()
  check_rows <- 0
  
  for(r in seq(1, nrow(x), by = 9)) {

    x9 <- x[r:(r+8),] %>% arrange(PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
    
    if (all(x9$sak_beidz == c("1", "2", "1", "2", "1", "2", "1", "2", "1")) && 
             diff(x9$NDZ_sanemsanas_datums[1:2]) == 0 &&
             all(diff(x9$NDZ_sanemsanas_datums[2:5]) != 0)) {
      x9_uzVieniniekiem <- rbind(x9_uzVieniniekiem, x9[9, ])
      x9_uzAstoniekiem <- rbind(x9_uzAstoniekiem, x9[1:8, ])
    } else if (all(x9$sak_beidz == c("2", "1", "2", "1", "1", "2", "2", "1", "1")) && 
               all(sapply(seq(1, 8, by = 2), function(i) all(diff(x9$NDZ_sanemsanas_datums[i:i+1]) == 0))) &&
               all(sapply(seq(2, 9, by = 2), function(i) all(diff(x9$NDZ_sanemsanas_datums[i:i+1]) != 0)))) {
      x9_uzVieniniekiem <- rbind(x9_uzVieniniekiem, x9[9, ])
      x9_uzAstoniekiem <- rbind(x9_uzAstoniekiem, x9[1:8, ])
    } else if (all(x9$sak_beidz == c("1", "2", "2", "1", "1", "2", "1", "2", "1")) && 
               all(sapply(seq(1, 4, by = 2), function(i) all(diff(x9$NDZ_sanemsanas_datums[i:i+1]) == 0))) &&
               all(diff(x9$NDZ_sanemsanas_datums[4:9]) != 0)) {
      x9_uzVieniniekiem <- rbind(x9_uzVieniniekiem, x9[9, ])
      x9_uzAstoniekiem <- rbind(x9_uzAstoniekiem, x9[1:8, ])
    } else if (all(x9$sak_beidz == c("1", "2", "1", "2", "2", "1", "1", "2", "1")) && 
               all(sapply(seq(1, 7, by = 2), function(i) all(diff(x9$NDZ_sanemsanas_datums[i:i+1]) == 0))) &&
               all(sapply(seq(2, 8, by = 2), function(i) all(diff(x9$NDZ_sanemsanas_datums[i:(i+1)]) != 0)))) {
       x9_uzVieniniekiem <- rbind(x9_uzVieniniekiem, x9[9, ])
       x9_uzAstoniekiem <- rbind(x9_uzAstoniekiem, x9[1:8, ])
    } else if (all(x9$sak_beidz == c("1", "2", "1", "2", "1", "2", "1", "2", "1"))) {
      x9_uzVieniniekiem <- rbind(x9_uzVieniniekiem, x9[9, ])
      x9_uzAstoniekiem <- rbind(x9_uzAstoniekiem, x9[1:8, ])
    } else if (all(x9$sak_beidz[c(1,2,7,8,9)] == c("1", "2", "2", "1", "1")) && diff(x9$NDZ_sanemsanas_datums[7:8]) == 0) {
      x9_uzVieniniekiem <- rbind(x9_uzVieniniekiem, x9[9, ])
      x9_uzAstoniekiem <- rbind(x9_uzAstoniekiem, x9[1:8, ])
    } else if (all(x9$sak_beidz[c(1,2,8,9)] == c("2", "1", "2", "1")) && 
               diff(x9$NDZ_sanemsanas_datums[1:2]) == 0 && 
               diff(x9$NDZ_sanemsanas_datums[8:9]) != 0) {
      x9_uzVieniniekiem <- rbind(x9_uzVieniniekiem, x9[9, ])
      x9_uzAstoniekiem <- rbind(x9_uzAstoniekiem, x9[1:8, ])
    } else if (all(x9$sak_beidz[c(1,2,8,9)] == c("1", "2", "2", "1"))) {
      if (diff(x9$NDZ_sanemsanas_datums[1:2]) != 0 && diff(x9$NDZ_sanemsanas_datums[8:9]) != 0) {
        x9_uzVieniniekiem <- rbind(x9_uzVieniniekiem, x9[9, ])
        x9_uzAstoniekiem <- rbind(x9_uzAstoniekiem, x9[1:8, ])
      } else if (all(sapply(c(2,4,8), function(i) diff(x9$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) && diff(x9$NDZ_sanemsanas_datums[1:2]) != 0) {
        if (x9$period[1] == "__________" && x9$PS_code[1] == "__________" && x9$NM_code[1] == "__________") {
          x9_uzVieniniekiem <- rbind(x9_uzVieniniekiem, x9[9, ])
          x9_uzAstoniekiem <- rbind(x9_uzAstoniekiem, x9[1:8, ])
        } else {stop("processingNines() iztrūkst apstrādes kods. \n")}
      } else {stop("processingNines() iztrūkst apstrādes kods. \n")}
    } else if (all(x9$sak_beidz[1:2] == c("2", "1")) && diff(x9$NDZ_sanemsanas_datums[1:2]) != 0) {
      x9_uzVieniniekiem <- rbind(x9_uzVieniniekiem, x9[1, ])
      x9_uzAstoniekiem <- rbind(x9_uzAstoniekiem, x9[2:9, ])
    } else if (all(x9$sak_beidz == c("2", "2", "1", "1", "2", "1", "2", "1", "2")) && all(diff(x9$NDZ_sanemsanas_datums) != 0)) {
      x9_uzVieniniekiem <- rbind(x9_uzVieniniekiem, x9[2, ])
      x9_uzSesi <- rbind(x9_uzSesi, x9[4:9, ])
    } else if (all(x9$sak_beidz == c("1", "2", "1", "1", "2", "1", "2", "1", "2"))) {
          if (diff(x9$NDZ_sanemsanas_datums[2:3]) == 0) {
            if (x9$period[1] == "__________" && x9$PS_code[1] == "__________" && x9$NM_code[1] == "__________") {
              x9_uzAstoniekiem <- rbind(x9_uzAstoniekiem, x9[c(1:2, 4:9), ])
            } else {stop("processingNines() iztrūkst apstrādes kods. \n")}
          } else {stop("processingNines() iztrūkst apstrādes kods. \n")}
    } else if (all(x9$sak_beidz[1:3] == c("1", "1", "2")) && x9$sak_beidz[4] != "2") {
            if (all(diff(x9$NDZ_sanemsanas_datums[1:2]) != 0)) {
                x9_uzDivi <- rbind(x9_uzDivi, x9[c(1,3), ])
                x9_uzSesi <- rbind(x9_uzSesi, x9[4:9, ])
            } else {stop("processingNines() iztrūkst apstrādes kods. \n")}
    } else if (all(x9$sak_beidz[1:4] == c("1", "2", "1", "2"))) {
            if (all(diff(x9$NDZ_sanemsanas_datums[1:4]) != 0)) {
              x9_uzDivi <- rbind(x9_uzDivi, x9[1:2, ])
              x9_uzSeptini <- rbind(x9_uzSeptini, x9[3:9, ])
            } else {stop("processingNines() iztrūkst apstrādes kods. \n")}
    } else {stop("processingNines() iztrūkst apstrādes kods: rindas ", r, ":", r+8, "!\n")}
    check_rows = check_rows + 9
  }
  
#PĀRBAUDE: Vai rindu skaits no deviņniekiem atvasinātajās tabulās sakrīt ar rindām izejas tabulā x.
if (nrow(x) == check_rows) {
  cat("PĀRBAUDE IZIETA: Apakštabulu rindu summa sakrīt ar izejošo devītnieku tabulu.\n")
  rm(x, x9)
} else {stop("PĀRBAUDE NAV IZIETA: Apakštabulu rindu summa NESAKRĪT ar izejošo devītnieku tabulu.\n")}
  
#1 Apakštabulu x9_uzVieniniekiem sūta caur processingOnes().
if(nrow(x9_uzVieniniekiem) > 0) {
    x9_uzVieniniekiem %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingOnes(o) %>% sendTo_tempNDZ(o)
} else {cat("Tabula x9_uzVieniniekiem ir tukša.\n")}
rm(x9_uzVieniniekiem)

#2 Apakštabulu x9_uzDivi sūta caur processingTwoes().
if(nrow(x9_uzDivi) > 0) {
  x9_uzDivi %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingTwoes(o, kods)
} else {cat("Tabula x9_uzDivi ir tukša.")}
rm(x9_uzDivi) 

#3 Apakštabulu x9_uzSesi sūta caur processingSixes().
if(nrow(x9_uzSesi) > 0) {
  x9_uzSesi %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingSixes(o, kods)
} else {cat("Tabula x9_uzSesi ir tukša.")}
rm(x9_uzSesi) 

#4 Apakštabulu x9_uzSeptini sūta caur processingSeven().
if(nrow(x9_uzSeptini) > 0) {
  x9_uzSeptini %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingSeven(o, kods)
} else {cat("Tabula x9_uzSeptini ir tukša.")}
rm(x9_uzSeptini) 

#4 Apakštabulu x9_uzAstoniekiem sūta caur processingEights().
if(nrow(x9_uzAstoniekiem) > 0) {
  x9_uzAstoniekiem %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingEights(o, kods)
} else {cat("Tabula x9_uzAstoniekiem ir tukša.")}
rm(x9_uzAstoniekiem) 
}

processingNines <- function(x, o) {
  x <- arrange(x, PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
  x9_uzVieniniekiem <- data.frame(); x9_uzSesi <- data.frame(); x9_uzAstoniekiem <- data.frame()
  check_rows <- 0
  
  for(r in seq(1, nrow(x), by = 9)) {

    x9 <- x[r:(r+8),] %>% arrange(PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
    
    if (all(x9$sak_beidz[1:2] == c("1", "2")) && 
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
    } else if (all(x9$sak_beidz[c(1,2,8,9)] == c("1", "2", "2", "1")) && 
               diff(x9$NDZ_sanemsanas_datums[1:2]) != 0 &&
               diff(x9$NDZ_sanemsanas_datums[8:9]) != 0) {
      x9_uzVieniniekiem <- rbind(x9_uzVieniniekiem, x9[9, ])
      x9_uzAstoniekiem <- rbind(x9_uzAstoniekiem, x9[1:8, ])
    } else if (all(x9$sak_beidz[1:2] == c("2", "1")) && diff(x9$NDZ_sanemsanas_datums[1:2]) != 0) {
      x9_uzVieniniekiem <- rbind(x9_uzVieniniekiem, x9[1, ])
      x9_uzAstoniekiem <- rbind(x9_uzAstoniekiem, x9[2:9, ])
    } else if (all(x9$sak_beidz == c("2", "2", "1", "1", "2", "1", "2", "1", "2")) && all(diff(x9$NDZ_sanemsanas_datums) != 0)) {
      x9_uzVieniniekiem <- rbind(x9_uzVieniniekiem, x9[2, ])
      x9_uzSesi <- rbind(x9_uzSesi, x9[4:9, ])
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
    x9_uzVieniniekiem %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingOnes(o) %>% sendTo_tempNDZ()
} else {cat("Tabula x9_uzVieniniekiem ir tukša.\n")}
rm(x9_uzVieniniekiem)

#2 Apakštabulu x9_uzSesi sūta caur processingSixes().
if(nrow(x9_uzSesi) > 0) {
  x9_uzSesi %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingSixes(o)
} else {cat("Tabula x9_uzSesi ir tukša.")}
rm(x9_uzSesi) 

#3 Apakštabulu x9_uzAstoniekiem sūta caur processingEights().
if(nrow(x9_uzAstoniekiem) > 0) {
  x9_uzAstoniekiem %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingEights(o)
} else {cat("Tabula x9_uzAstoniekiem ir tukša.")}
rm(x9_uzAstoniekiem) 
}

processingNines <- function(x, o) {
  x <- arrange(x, PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
  
  x9_uzVieniniekiem <- data.frame()
  x9_uzAstoniekiem <- data.frame()
  
  for (r in seq(1, nrow(x), by = 9)) {

    x9 <- x[r : (r + 8), ]
    x9 <- arrange(x9, PS_code, DN_code, NM_code,NDZ_sanemsanas_datums)
    
    if (all(x9$sak_beidz[1:2] == c("1", "2")) && 
             diff(x9$NDZ_sanemsanas_datums[1:2]) == 0 &&
             all(diff(x9$NDZ_sanemsanas_datums[2:5]) != 0)) {
      x9_uzVieniniekiem <- rbind(x9_uzVieniniekiem, x9[9, ])
      x9_uzAstoniekiem <- rbind(x9_uzAstoniekiem, x9[1:8, ])
    } else if (all(x9$sak_beidz[c(2, 4, 5, 8, 9)] == "1") && all(x9$sak_beidz[c(1,3,6,7)] == "2") && 
               all(sapply(seq(1, 8, by = 2), function(i) all(diff(x9$NDZ_sanemsanas_datums[i:i+1]) == 0))) &&
               all(sapply(seq(2, 9, by = 2), function(i) all(diff(x9$NDZ_sanemsanas_datums[i:i+1]) != 0)))) {
      x9_uzVieniniekiem <- rbind(x9_uzVieniniekiem, x9[9, ])
      x9_uzAstoniekiem <- rbind(x9_uzAstoniekiem, x9[1:8, ])
      
    } else if (all(x9$sak_beidz[c(1, 4, 5, 7, 9)] == "1") && all(x9$sak_beidz[c(2,3,6,8)] == "2") && 
               all(sapply(seq(1, 4, by = 2), function(i) all(diff(x9$NDZ_sanemsanas_datums[i:i+1]) == 0))) &&
               all(diff(x9$NDZ_sanemsanas_datums[4:9]) != 0)) {
      x9_uzVieniniekiem <- rbind(x9_uzVieniniekiem, x9[9, ])
      x9_uzAstoniekiem <- rbind(x9_uzAstoniekiem, x9[1:8, ])
    } else if (all(x9$sak_beidz[c(1, 3, 6, 7, 9)] == "1") && all(x9$sak_beidz[c(2, 4, 5, 8)] == "2") && 
               all(sapply(seq(1, 7, by = 2), function(i) all(diff(x9$NDZ_sanemsanas_datums[i:i+1]) == 0))) &&
               all(sapply(seq(2, 8, by = 2), function(i) all(diff(x9$NDZ_sanemsanas_datums[i:(i+1)]) != 0)))) {
       x9_uzVieniniekiem <- rbind(x9_uzVieniniekiem, x9[9, ])
       x9_uzAstoniekiem <- rbind(x9_uzAstoniekiem, x9[1:8, ])
    } else if (all(x9$sak_beidz[c(1, 3, 5, 7, 9)] == "1") && all(x9$sak_beidz[c(2, 4, 6, 8)] == "2")) {
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
    #} else if((((sum(x9$start == "1") == 4 &&sum(x9$end == "2") == 5) && (x9$end[1] == "2" && x9$NDZ_sanemsanas_datums[1] != x9$NDZ_sanemsanas_datums[2])) && ((((x9$start[2] == "1" && x9$end[3] == "2") && x9$NDZ_sanemsanas_datums[2] <= x9$NDZ_sanemsanas_datums[3])||((x9$end[2] == "2" && x9$start[3] == "1") && x9$NDZ_sanemsanas_datums[2] == x9$NDZ_sanemsanas_datums[3])) && (((x9$start[4] == "1" && x9$end[5] == "2") && x9$NDZ_sanemsanas_datums[4] <= x9$NDZ_sanemsanas_datums[5])||((x9$end[4] == "2" && x9$start[5] == "1") && x9$NDZ_sanemsanas_datums[4] == x9$NDZ_sanemsanas_datums[5])))) && ((((x9$start[6] == "1" && x9$end[7] == "2") && x9$NDZ_sanemsanas_datums[6] <= x9$NDZ_sanemsanas_datums[7])||((x9$end[6] == "2" && x9$start[7] == "1") && x9$NDZ_sanemsanas_datums[6] == x9$NDZ_sanemsanas_datums[7])) && (((x9$start[8] == "1" && x9$end[9] == "2") && x9$NDZ_sanemsanas_datums[8] <= x9$NDZ_sanemsanas_datums[9])||((x9$end[8] == "2" && x9$start[9] == "1") && x9$NDZ_sanemsanas_datums[8] == x9$NDZ_sanemsanas_datums[9])))){
    #  x9_uzVieniniekiem <- rbind(x9_uzVieniniekiem, x9[1, ])
    #  x9_uzAstoniekiem <- rbind(x9_uzAstoniekiem, x9[2:9, ])
    } else if (all(x9$sak_beidz[1:2] == c("2", "1")) && diff(x9$NDZ_sanemsanas_datums[1:2]) != 0) {
      x9_uzVieniniekiem <- rbind(x9_uzVieniniekiem, x9[1, ])
      x9_uzAstoniekiem <- rbind(x9_uzAstoniekiem, x9[2:9, ])
    } else {stop("processingNines() iztrūkst apstrādes kods: rindas ", r, ":", r+8, "!\n")}
  }
  
# PĀRBAUDE: Vai rindu skaits no deviņniekiem atvasinātajās tabulās sakrīt ar rindām izejas tabulā x.
  if ((nrow(x9_uzAstoniekiem) + nrow(x9_uzVieniniekiem)) == nrow(x)) {
    cat("PĀRBAUDE IZIETA: Apakštabulu rindu summa sakrīt ar izejošo devītnieku tabulu.\n")
    rm(x, x9)
  } else {
    stop("PĀRBAUDE NAV IZIETA: Apakštabulu rindu summa NESAKRĪT ar izejošo devītnieku tabulu.\n")
  }
  
#1 Apakštabulu x9_uzVieniniekiem sūta caur processingOnes().
if(nrow(x9_uzVieniniekiem) > 0) {
   x9_uzVieniniekiem <- arrange(x9_uzVieniniekiem, PS_code, NM_code, NDZ_sanemsanas_datums)
   cat("No devītniekiem atvasinātā tabula x9_uzVieniniekiem pārsūtīta uz processingOnes() un tad uz tempNDZ, ko būvējam.\n")
    sendTo_tempNDZ(processingOnes(x9_uzVieniniekiem, o))
} else {cat("Tabula x9_uzVieniniekiem ir tukša.\n")}
  rm(x9_uzVieniniekiem)
  
#2 Apakštabulu x9_uzAstoniekiem sūta caur processingEights().
if(nrow(x9_uzAstoniekiem) > 0) {
  x9_uzAstoniekiem <- arrange(x9_uzAstoniekiem, PS_code, NM_code, NDZ_sanemsanas_datums)
  cat("No devītniekiem atvasinātā tabula x9_uzAstoniekiem pārsūtīta uz processingEights() un tad uz tempNDZ, ko būvējam.\n")
  processingEights(x9_uzAstoniekiem, o)
} else {cat("Tabula x9_uzAstoniekiem ir tukša.")}
  rm(x9_uzAstoniekiem) 
}

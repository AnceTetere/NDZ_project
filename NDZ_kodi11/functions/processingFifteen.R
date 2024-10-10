processingFifteen <- function(x, o) {
  x <- arrange(x, PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
  
  x15_uzVieniniekiem <- data.frame()
  x15_uzDivniekiem <- data.frame()
  x15_uzSesi <- data.frame()
  x15_uzCetrpadsmit <- data.frame()
  check_rows <- 0
  
  for (r in seq(1, nrow(x), by = 15)) {
    
    x15 <- x[r:(r+14),]
    x15 <- arrange(x15, PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
    
    if (all(sapply(seq(1,15,by=2), function(i) x15$sak_beidz[i] == "1")) && 
        diff(x15$NDZ_sanemsanas_datums[14:15]) != 0) {
      x15_uzVieniniekiem <- rbind(x15_uzVieniniekiem, x15[15, ])    
      x15_uzCetrpadsmit <- rbind(x15_uzCetrpadsmit, x15[-15, ])    
    } else if (all(sapply(seq(1,15,by=2), function(i) x15$sak_beidz[i] == "2")) && 
               diff(x15$NDZ_sanemsanas_datums[14:15]) != 0) {
      x15_uzVieniniekiem <- rbind(x15_uzVieniniekiem, x15[1, ])    
      x15_uzCetrpadsmit <- rbind(x15_uzCetrpadsmit, x15[-1, ])    
    } else if (all(x15$zinkod == c("40", "91", "41", "92", "40", "91", "41", "92", "40", "91", "41", "92", "40", "91", "92")) &&
               all(sapply(seq(1,13,by=2), function(i) diff(x15$NDZ_sanemsanas_datums[i:(i+1)]) == 0))) {
      x15_uzVieniniekiem <- rbind(x15_uzVieniniekiem, x15[c(1,15), ])    
      x15_uzSesi <- rbind(x15_uzSesi, x15[seq(3,13,by=2), ])    
    } else {stop("15-nieku tabulas pārdalei trūkst izstrādes koda. Rindas: ", r, " līdz ", r + 14, "\n")}
    check_rows <- check_rows + 15
  }
  
#PĀRBAUDE: Vai rindu skaits no 15-niekiem atvasinātajās tabulās sakrīt ar rindām izejas tabulā x.
  if (check_rows == nrow(x)) {
    cat("PĀRBAUDE IZIETA: Apakštabulu rindu summa sakrīt ar izejošo 15-nieku tabulu.\n"); rm(x, x15, r)
  } else {stop("ERROR: Apakštabulu rindu summa NESAKRĪT ar izejošo 15-nieku tabulu.\n")}

#1 Apakštabulu x15_uzVieniniekiem sūta caur processingOnes().
  if (nrow(x15_uzVieniniekiem) > 0) {
    x15_uzVieniniekiem <- arrange(x15_uzVieniniekiem, PS_code, NM_code, NDZ_sanemsanas_datums)
    cat("No 15-niekiem atvasinātā tabula x15_uzVieniniekiem pārsūtīta uz processingOnes() un tad uz tempNDZ, ko būvējam.\n")
    sendTo_tempNDZ(processingOnes(x15_uzVieniniekiem, o))
  } else {cat("Tabula x15_uzVieniniekiem ir tukša.\n")}
  rm(x15_uzVieniniekiem)
  
#2 Apakštabulu x15_uzSesi sūta caur processingSixes().
  if (nrow(x15_uzSesi) > 0) {
    x15_uzSesi <- arrange(x15_uzSesi, PS_code, NM_code, NDZ_sanemsanas_datums)
    cat("No 15-niekiem atvasinātā tabula x15_uzSesi pārsūtīta uz processingSixes() un tad uz tempNDZ, ko būvējam.\n")
    processingSixes(x15_uzSesi, o)
  } else {cat("Tabula x15_uzSesi ir tukša.\n")}
  rm(x15_uzSesi)
      
#3 Apakštabulu x15_uzCetrpadsmit sūta caur processingFourteen().
  if (nrow(x15_uzCetrpadsmit) > 0) {
    x15_uzCetrpadsmit <- arrange(x15_uzCetrpadsmit, PS_code, NM_code, NDZ_sanemsanas_datums)
    cat("No 14-niekiem atvasinātā tabula x15_uzCetrpadsmit pārsūtīta uz processingFourteen() un caur to uz tempNDZ, ko būvējam.\n")
    processingFourteen(x15_uzCetrpadsmit, o)
  } else {cat("Tabula x15_uzCetrpadsmit ir tukša.\n")}
  rm(x15_uzCetrpadsmit)
}

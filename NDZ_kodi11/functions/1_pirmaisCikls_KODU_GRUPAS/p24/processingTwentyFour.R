processingTwentyFour <- function(x, o, kods) {
  x <- x[order(x$period, x$PS_code, x$DN_code, x$NM_code, x$NDZ_sanemsanas_datums, x$zinkod), ] 
  
  x24_uzVieniniekiem <- data.frame()
  x24_uzDesmit <- data.frame()
  x24_uzTrispadsmit <- data.frame()
  
  for (r in seq(1, nrow(x), by = 24)) {
  
    x24 <- x[r:(r+23),]
    x24 <- x24[order(x24$period, x24$PS_code, x24$DN_code, x24$NM_code, x24$NDZ_sanemsanas_datums, x24$zinkod), ]
    
    if (all(sapply(seq(2, 24, by = 2), function(i) x24$sak_beidz[i] == "1")) && 
        all(diff(x24$NDZ_sanemsanas_datums) != 0)) {
      x24_uzVieniniekiem <- rbind(x24_uzVieniniekiem, x24[1, ])    
      x24_uzDesmit <- rbind(x24_uzDesmit, x24[2:11, ])    
      x24_uzTrispadsmit <- rbind(x24_uzTrispadsmit, x24[12:24, ])
    } else {
      stop("24-nieku tabulas pārdalei trūkst izstrādes koda. Rindas: ", r, " līdz ", r + 13, "\n")
    }
  }
  
  #2 PĀRBAUDE: Vai rindu skaits no 15-niekiem atvasinātajās tabulās sakrīt ar rindām izejas tabulā x.
  if (nrow(x24_uzVieniniekiem) + nrow(x24_uzDesmit) + nrow(x24_uzTrispadsmit) == nrow(x)) {
    cat("PĀRBAUDE IZIETA: Apakštabulu rindu summa sakrīt ar izejošo 24-nieku tabulu.\n")
    rm(x, x24, r)
  } else {
    stop("ERROR: Apakštabulu rindu summa NESAKRĪT ar izejošo 24-nieku tabulu.\n")
  }
  
  #3 Apakštabulu x24_uzVieniniekiem sūta caur processingOnes().
  if (nrow(x24_uzVieniniekiem) > 0) {
    x24_uzVieniniekiem <- x24_uzVieniniekiem[order(x24_uzVieniniekiem$PS_code, x24_uzVieniniekiem$NM_code, x24_uzVieniniekiem$NDZ_sanemsanas_datums, x24_uzVieniniekiem$zinkod),]
    cat("No 24-niekiem atvasinātā tabula x24_uzVieniniekiem pārsūtīta uz processingOnes() un tad uz tempNDZ, ko būvējam.\n")
    sendTo_tempNDZ(processingOnes(x24_uzVieniniekiem, o))
  } else {
    cat("Tabula x24_uzVieniniekiem ir tukša.\n")
  }
  
  rm(x24_uzVieniniekiem)
  
  #4 Apakštabulu x24_uzDesmit sūta caur processingTens().
  if (nrow(x24_uzDesmit) > 0) {
    x24_uzDesmit <- x24_uzDesmit[order(x24_uzDesmit$PS_code, x24_uzDesmit$NM_code, x24_uzDesmit$NDZ_sanemsanas_datums, x24_uzDesmit$zinkod),]
    cat("No 24-niekiem atvasinātā tabula x24_uzDesmit pārsūtīta uz processingTens() un caur to uz tempNDZ, ko būvējam.\n")
    processingTens(x24_uzDesmit, o, kods)
  } else {
    cat("Tabula x24_uzDesmit ir tukša.\n")
  }
  
  rm(x24_uzDesmit)
  
  #5 Apakštabulu x24_uzTrispadsmit sūta caur processingThirteen().
  if (nrow(x24_uzTrispadsmit) > 0) {
    x24_uzTrispadsmit <- x24_uzTrispadsmit[order(x24_uzTrispadsmit$PS_code, x24_uzTrispadsmit$NM_code, x24_uzTrispadsmit$NDZ_sanemsanas_datums, x24_uzTrispadsmit$zinkod),]
    cat("No 24-niekiem atvasinātā tabula x24_uzTrispadsmit pārsūtīta uz processingThirteen() un caur to uz tempNDZ, ko būvējam.\n")
    processingThirteen(x24_uzTrispadsmit, o, kods)
  } else {
    cat("Tabula x24_uzTrispadsmit ir tukša.\n")
  }
  
  rm(x24_uzTrispadsmit)
}

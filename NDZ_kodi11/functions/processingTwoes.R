processingTwoes <- function(x, o) {
  
  #1 Sakārto tabulu
  x <- arrange(x, PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
  rownames(x) <- NULL
  
  x2_uzVieniniekiem <- data.frame()
  x2_trueDoubles <- data.frame()
  
  for (r in seq(1, nrow(x), by = 2)) {
    if (doublesTest(r, x)) { 
      if ((x$sak_beidz[r] == "2" && x$sak_beidz[r+1] == "1" && diff(x$NDZ_sanemsanas_datums[r:(r+1)]) > 0) || 
          (x$sak_beidz[r] == x$sak_beidz[r + 1])) {
        x2_uzVieniniekiem <- rbind(x2_uzVieniniekiem, x[c(r, r + 1),])
      } else {x2_trueDoubles <- rbind(x2_trueDoubles, x[c(r, r + 1),])}
    } else {stop("Rindās ", r, " un ", r + 1, " pamatkodi atšķiras.\n")}
  }
      
  rm(x, r)
  
  #2 Tabulu x2_uzVieniniekiem sūta uz vieninieku apstrādi caur processingOnes().
  if(nrow(x2_uzVieniniekiem) > 0) {
    sendTo_tempNDZ(processingOnes(x2_uzVieniniekiem, o))
  } else {
    cat("No divniekiem atvasinātajā vieninieku tabulā nav nevienas rindas.\n")
  }
  rm(x2_uzVieniniekiem)

#3 Izstrādā x2_trueDoubles
#3.1 Pārbaude vai šie visi ir īstie dubultnieki
  
  if (nrow(x2_trueDoubles) > 0) {
    y <- arrange(x2_trueDoubles, PS_code, NM_code); test <- 0
    
    for (r in seq(1, nrow(y), by = 2)) {if (doublesTest(r, y)) {test <- test + 2}}
    
    if (test == nrow(y)) {cat("PĀRBAUDE IZIETA: \n Tabulā visi ir īstie dubultnieki ar sākuma un beigu kodu. \n")
      rm(y, test, r)
    } else {stop("ProcessingTwoes: PĀRBAUDE NAV IZIETA\n Tabulā NAV tikai dubultnieki! \n")}
    
#3.2 Atvasinātos un pārbaudītos pārus sūta apstrādei caur funkciju processingDoubles()
  sendTo_tempNDZ(processingDoubles(x2_trueDoubles, o)) 
    
  } else {
    cat("Tabula x2_trueDoubles ir tukša")
  }
  rm(x2_trueDoubles)
}

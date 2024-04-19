processingTwoes <- function(x, o) {

  x <- x[order(x$PS_code, x$DN_code, x$NM_code, x$NDZ_sanemsanas_datums, x$start, x$end), ]
  x2_uzVieniniekiem <- data.frame()
  x2_trueDoubles <- data.frame()
  
  for (r in seq(1, nrow(x), by = 2)) {
    if (doublesTest(r, x)) {
      if ((x$end[r] == "2" && x$start[r+1] == "1" && x$NDZ_sanemsanas_datums[r+1] > x$NDZ_sanemsanas_datums[r]) || 
          (x$start[r] == x$start[r + 1] || x$end[r] == x$end[r + 1]) || 
          (((x$zinkod[r] == "40" || x$zinkod[r+1] == "41") || (x$zinkod[r] == "50" || x$zinkod[r+1] == "51") || (x$zinkod[r] == "53" || x$zinkod[r+1] == "54") || (x$zinkod[r] == "91" || x$zinkod[r+1] == "92")) && x$NDZ_sanemsanas_datums[r] != x$NDZ_sanemsanas_datums[r+1])) {
        x2_uzVieniniekiem <- rbind(x2_uzVieniniekiem, x[c(r, r + 1),])    # 1586 rows
      } else {
        x2_trueDoubles <- rbind(x2_trueDoubles, x[c(r, r + 1),])
        if (!(x$PS_code[r] == x$PS_code[r + 1] &&
              x$DN_code[r] == x$DN_code[r + 1]) &&
            (x$NM_code[r] == x$NM_code[r + 1])) {
          stop(cat("Rindās ", r, " un ", r + 1, " ir nesakritības.\n"))
        }
      }
    } else {
      cat("Rindās", r, "un", r + 1, "pamatkodi atšķiras.\n")
    }
  }
  
  rm(x, r)
  
  #7 tabulu x2_uzVieniniekiem sūta uz vieninieku apstrādi.
  #7.1 Pārbaude vai nav dubultnieki
  
  if(nrow(x2_uzVieniniekiem) > 0) {
    sendTo_tempNDZ(processingOnes(x2_uzVieniniekiem, o))
  } else {
    cat("No divniekiem atvasinātajā vieninieku tabulā nav nevienas rindas.\n")
  }
  rm(x2_uzVieniniekiem)

  #8 Izstrādā x2_trueDoubles
  #8.1. Pārbaude vai šie visi ir īstie dubultnieki
  
  if (nrow(x2_trueDoubles) > 0) {
    y <-
      x2_trueDoubles[order(x2_trueDoubles$PS_code, x2_trueDoubles$NM_code),]
    test <- 0
    
    for (r in seq(1, nrow(y), by = 2)) {
      if (doublesTest(r, y))
        test <- test + 2
    }
    if (test == nrow(y)) {
      cat("PĀRBAUDE IZIETA: \n
      Tabulā visi ir īstie dubultnieki ar sākuma un beigu kodu")
      rm(y, test, r)
    } else {
      stop(cat("PĀRBAUDE NAV IZIETA:\n
           Tabulā NAV tikai dubultnieki!"))
    }
    
    sendTo_tempNDZ(processingDoubles(x2_trueDoubles, o)) 
  } else {
    cat("Tabula x2_trueDoubles ir tukša")
  }
  rm(x2_trueDoubles)
}

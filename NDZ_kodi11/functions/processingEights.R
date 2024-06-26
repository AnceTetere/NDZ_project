processingEights <- function(x, o) {
  x <- x[order(x$PS_code, x$DN_code, x$NM_code, x$NDZ_sanemsanas_datums), ]

  x8_uzVieniniekiem <- data.frame()
  x8_uzCetriniekiem <- data.frame()
  x8_uzSesi <- data.frame()
  x8_uzSeptini <- data.frame()
  check_rows <- 0
  
  for (r in seq(1, nrow(x), by = 8)) {
    x8 <- x[r:(r+7), ]
    x8 <- x8[order(x8$PS_code, x8$DN_code, x8$NM_code, x8$NDZ_sanemsanas_datums), ]

    if (sum(x8$start == "1") == 4) {
        result <- processingEights_s4(x8)
    
        x8_uzVieniniekiem <- rbind(x8_uzVieniniekiem, result$x8s4_uzVieniniekiem)
        x8_uzCetriniekiem <- rbind(x8_uzCetriniekiem, result$x8s4__uzCetriniekiem)
        x8_uzSesi <- rbind(x8_uzSesi, result$x8s4_uzSesi)
        x8_uzSeptini <- rbind(x8_uzSeptini, result$x8s4_uzSeptini)
        
    } else if (x8$end[1] == "2" && x8$start[2] == "1" && x8$NDZ_sanemsanas_datums[1] != x8$NDZ_sanemsanas_datums[2]) {
      x8_uzVieniniekiem <- rbind(x8_uzVieniniekiem, x8[1, ])
      x8_uzSeptini <- rbind(x8_uzSeptini, x8[-1, ])
    } else if (x8$start[1] == x8$start[2] && all(diff(x8$NDZ_sanemsanas_datums[1:3]) != 0)) {
      x8_uzSeptini <- rbind(x8_uzSeptini, x8[2:8, ])
    } else if (x8$start[1] == "1" && x8$end[2] == "2" && x8$end[3] == "2" && x8$start[4] == "1" && 
               all(diff(x8$NDZ_sanemsanas_datums[1:3]) != 0) && all(diff(x8$NDZ_sanemsanas_datums[4:6]) != 0) &&
               all(diff(x8$NDZ_sanemsanas_datums[3:4]) == 0) && all(diff(x8$NDZ_sanemsanas_datums[6:7]) == 0)) {
      x8_uzCetriniekiem <- rbind(x8_uzCetriniekiem, x8)
    } else if (all(x8$start[c(1, 3:4, 6:8)] == "1") && 
               all(diff(x8$NDZ_sanemsanas_datums[1:7]) != 0) && all(diff(x8$NDZ_sanemsanas_datums[7:8]) == 0) &&
               x8$PS_code[1] == '_________' && x8$NM_code[1] == '___________') {
      x8_uzVieniniekiem <- rbind(x8_uzVieniniekiem, x8[1,])
    } else if (all(x8$start[c(1, 3, 5, 7, 8)] == "1") && all(diff(x8$NDZ_sanemsanas_datums) != 0)){
      x8_uzVieniniekiem <- rbind(x8_uzVieniniekiem, x8[8,])
      x8_uzSesi <- rbind(x8_uzSesi, x8[1:6,])
    } else if (sum(x8$start == "1") == 5) { 
      if(all(x8$start[c(1, 3, 5, 7, 8)] == "1") && 
         all(sapply(c(3, 6), function(i) all(diff(x8$NDZ_sanemsanas_datums[i:(i+1)]) == 0))) &&
         all(sapply(c(1,2,4,5,7), function(i) all(diff(x8$NDZ_sanemsanas_datums[i:(i+1)]) != 0)))) {
        x8_uzVieniniekiem <- rbind(x8_uzVieniniekiem, x8[8,])
        x8_uzSesi <- rbind(x8_uzSesi, x8[1:6,])
      } else {stop("processingEights: Iztrūkst kods rindām ", r, " līdz ", r + 7,".\n")}
    } else {
      stop("processingEights: Iztrūkst kods rindām ", r, " līdz ", r + 7,".\n")
    }
    check_rows <- check_rows + 8
  }
  
  #2 PĀRBAUDE: Vai rindu skaits no astoņniekiem atvasinātajās tabulās sakrīt ar rindām izejas tabulā x.
  if (check_rows == nrow(x)) {
    cat("PĀRBAUDE IZIETA: Apakštabulu rindu summa sakrīt ar izejošo tabulu.\n")
    rm(x, x8, check_rows)
  } else {
    stop(cat("ERROR: Pārbaude nav izieta. Apakštabulu rindu summa NESAKRĪT ar izejošo tabulu.\n"))
  }
  
  #4) Apakštabulu x8_uzVieniniekiem sūta caur processingOnes().
  if(nrow(x8_uzVieniniekiem) > 0) {
    x8_uzVieniniekiem <- x8_uzVieniniekiem [order(x8_uzVieniniekiem$PS_code, x8_uzVieniniekiem$NM_code, x8_uzVieniniekiem$NDZ_sanemsanas_datums), ]
    sendTo_tempNDZ(processingOnes(x8_uzVieniniekiem, o))
    cat("No astoņniekiem atvasinātā tabula x8_uzVieniniekiem pārsūtīta apstrādei caur processingOnes().\n")
  } else {
    cat("Tabula x8_uzVieniniekiem ir tukša.\n")
  }
  
  rm(x8_uzVieniniekiem)
  
  #5) Apakštabulu x8_uzCetriniekiem sūta caur processingFours.
  if(nrow(x8_uzCetriniekiem) > 0) {
    x8_uzCetriniekiem <- x8_uzCetriniekiem[order(x8_uzCetriniekiem$PS_code, x8_uzCetriniekiem$NM_code, x8_uzCetriniekiem$NDZ_sanemsanas_datums), ]
    processingFours(x8_uzCetriniekiem, o)
    cat("No astoņniekiem atvasinātā tabula x8_uzCetriniekiem pārsūtīta apstrādei caur processingFours.\n")
  } else {
    cat("Tabula x8_uzCetriniekiem ir tukša.\n")
  }
  
  rm(x8_uzCetriniekiem)
  
  #6) Apakštabulu x8_uzSesi sūta caur processingSixes().
  if(nrow(x8_uzSesi) > 0) {
    x8_uzSesi <- x8_uzSesi[order(x8_uzSesi$PS_code, x8_uzSesi$NM_code, x8_uzSesi$NDZ_sanemsanas_datums), ]
    processingSixes(x8_uzSesi, o)
    cat("No astoņniekiem atvasinātā tabula x8_uzSesi pārsūtīta apstrādei caur processingSixes.\n")
  } else {
    cat("Tabula x8_uzSesi ir tukša.\n")
  }
  
  rm(x8_uzSesi)
  
  #7) Apakštabulu x8_uzSeptini sūta caur processingSeven().
  if(nrow(x8_uzSeptini) > 0) {
    x8_uzSeptini <- x8_uzSeptini[order(x8_uzSeptini$PS_code, x8_uzSeptini$NM_code, x8_uzSeptini$NDZ_sanemsanas_datums), ]
    processingSeven(x8_uzSeptini, o)
    cat("No astoņniekiem atvasinātā tabula x8_uzSeptini pārsūtīta apstrādei caur processingSeven.\n")
  } else {
    cat("Tabula x8_uzSeptini ir tukša.\n")
  }
  
  rm(x8_uzSeptini)
}

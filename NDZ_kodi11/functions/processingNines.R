processingNines <- function(x, o) {
  x <- x[order(x$period, x$PS_code, x$DN_code, x$NM_code, x$NDZ_code_sanemsanas_datums), ]
  
  x9_uzVieniniekiem <- data.frame()
  x9_uzAstoniekiem <- data.frame()
  
  for (r in seq(1, nrow(x), by = 9)) {
    #TESTĒŠANAI r <- sample(1: nrow(x), size = 1, replace = FALSE)
    x9 <- x[r : (r + 8), ]
    x9 <- x9[order(x9$period, x9$PS_code, x9$DN_code, x9$NM_code, x9$NDZ_code_sanemsanas_datums), ]
    # TESTĒŠANAI: x[x$PS_code == x$PS_code[r], ]
    
    if (((x9$start[1] == "1" && x9$start[3] == "1") && ((x9$start[5] == "1" && x9$start[7] == "1") && (x9$start[9] == "1" && x9$end[2] == "2")))&&((x9$end[4] == "2" && x9$end[6] == "2") && x9$end[8] == "2")) {
      x9_uzVieniniekiem <- rbind(x9_uzVieniniekiem, x9[9, ])
      x9_uzAstoniekiem <- rbind(x9_uzAstoniekiem, x9[1:8, ])
    } else if((((sum(x9$start == "1") == 4 &&sum(x9$end == "2") == 5) && (x9$end[1] == "2" && x9$NDZ_code_sanemsanas_datums[1] != x9$NDZ_code_sanemsanas_datums[2])) && ((((x9$start[2] == "1" && x9$end[3] == "2") && x9$NDZ_code_sanemsanas_datums[2] <= x9$NDZ_code_sanemsanas_datums[3])||((x9$end[2] == "2" && x9$start[3] == "1") && x9$NDZ_code_sanemsanas_datums[2] == x9$NDZ_code_sanemsanas_datums[3])) && (((x9$start[4] == "1" && x9$end[5] == "2") && x9$NDZ_code_sanemsanas_datums[4] <= x9$NDZ_code_sanemsanas_datums[5])||((x9$end[4] == "2" && x9$start[5] == "1") && x9$NDZ_code_sanemsanas_datums[4] == x9$NDZ_code_sanemsanas_datums[5])))) && ((((x9$start[6] == "1" && x9$end[7] == "2") && x9$NDZ_code_sanemsanas_datums[6] <= x9$NDZ_code_sanemsanas_datums[7])||((x9$end[6] == "2" && x9$start[7] == "1") && x9$NDZ_code_sanemsanas_datums[6] == x9$NDZ_code_sanemsanas_datums[7])) && (((x9$start[8] == "1" && x9$end[9] == "2") && x9$NDZ_code_sanemsanas_datums[8] <= x9$NDZ_code_sanemsanas_datums[9])||((x9$end[8] == "2" && x9$start[9] == "1") && x9$NDZ_code_sanemsanas_datums[8] == x9$NDZ_code_sanemsanas_datums[9])))){
      x9_uzVieniniekiem <- rbind(x9_uzVieniniekiem, x9[1, ])
      x9_uzAstoniekiem <- rbind(x9_uzAstoniekiem, x9[2:9, ])
    } else if (x9$end[1] == "2" && x9$start[2] == "1" && x9$NDZ_code_sanemsanas_datums[1] != x9$NDZ_code_sanemsanas_datums[2]) {
      x9_uzVieniniekiem <- rbind(x9_uzVieniniekiem, x9[1, ])
      x9_uzAstoniekiem <- rbind(x9_uzAstoniekiem, x9[2:9, ])
    } else if (x9$start[1] == "1" && x9$end[2] == "2" && x9$end[7] == "2" && x9$start[8] == "1" && x9$NDZ_code_sanemsanas_datums[7] == x9$NDZ_code_sanemsanas_datums[8] && x9$start[9] == "1") {
      x9_uzVieniniekiem <- rbind(x9_uzVieniniekiem, x9[9, ])
      x9_uzAstoniekiem <- rbind(x9_uzAstoniekiem, x9[1:8, ])
    } else if (x9$end[1] == "2" && x9$start[2] == "1" && 
               x9$NDZ_code_sanemsanas_datums[1] == x9$NDZ_code_sanemsanas_datums[2] &&
               x9$end[8] == "2" && x9$start[9] == "1" && 
               x9$NDZ_code_sanemsanas_datums[8] != x9$NDZ_code_sanemsanas_datums[9]) {
      x9_uzVieniniekiem <- rbind(x9_uzVieniniekiem, x9[9, ])
      x9_uzAstoniekiem <- rbind(x9_uzAstoniekiem, x9[1:8, ])
    } else if (x9$start[1] == "1" && x9$end[2] == "2" && 
               x9$NDZ_code_sanemsanas_datums[1] != x9$NDZ_code_sanemsanas_datums[2] &&
               x9$end[8] == "2" && x9$start[9] == "1" && 
               x9$NDZ_code_sanemsanas_datums[8] != x9$NDZ_code_sanemsanas_datums[9]) {
      x9_uzVieniniekiem <- rbind(x9_uzVieniniekiem, x9[9, ])
      x9_uzAstoniekiem <- rbind(x9_uzAstoniekiem, x9[1:8, ])
    } else {
      stop(cat("Deviņnieku izstrādē processingNines() gadījums, kas atrodams izejas tabulas x rindās",  r, "līdz", r+8, "nav izstrādāts!\n"))
    }
  }
  
  #2 PĀRBAUDE: Vai rindu skaits no deviņniekiem atvasinātajās tabulās sakrīt ar rindām izejas tabulā x.
  if ((nrow(x9_uzAstoniekiem) + nrow(x9_uzVieniniekiem)) == nrow(x)) {
    cat("PĀRBAUDE IZIETA: Apakštabulu rindu summa sakrīt ar izejošo devītnieku tabulu.\n")
    rm(x, x9)
  } else {
    stop(cat("PĀRBAUDE NAV IZIETA: Apakštabulu rindu summa NESAKRĪT ar izejošo devītnieku tabulu.\n"))
  }
  
  #3 Apakštabulu x9_uzVieniniekiem sūta caur processingOnes().
  if(nrow(x9_uzVieniniekiem) > 0) {
    x9_uzVieniniekiem <- x9_uzVieniniekiem[order(x9_uzVieniniekiem$PS_code, x9_uzVieniniekiem$NM_code, x9_uzVieniniekiem$NDZ_code_sanemsanas_datums), ]
    cat("No devītniekiem atvasinātā tabula x9_uzVieniniekiem pārsūtīta uz processingOnes un tad uz tempNDZ_code, ko būvējam.\n")
    sendTo_tempNDZ_code(processingOnes(x9_uzVieniniekiem, o))
  } else {
    cat("Tabula x9_uzVieniniekiem ir tukša.\n")
  }
  rm(x9_uzVieniniekiem)
  
  #4 Apakštabulu x9_uzAstoniekiem sūta caur processingEights().
  if(nrow(x9_uzAstoniekiem) > 0) {
    x9_uzAstoniekiem <- x9_uzAstoniekiem[order(x9_uzAstoniekiem$PS_code, x9_uzAstoniekiem$NM_code, x9_uzAstoniekiem$NDZ_code_sanemsanas_datums), ]
    cat("No devītniekiem atvasinātā tabula x9_uzAstoniekiem pārsūtīta uz processingEights un tad uz tempNDZ_code, ko būvējam.")
    processingEights(x9_uzAstoniekiem, o)
  } else {
    cat("Tabula x9_uzAstoniekiem ir tukša.")
  }
  rm(x9_uzAstoniekiem) 
}

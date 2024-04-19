processingThirteen <- function(x, o) {
  x <- x[order(x$period, x$PS_code, x$DN_code, x$NM_code, x$NDZ_sanemsanas_datums, x$start), ]

  x13_uzVieniniekiem <- data.frame()
  x13_uzTrijniekiem <- data.frame()
  x13_uzCetriniekiem <- data.frame()
  x13_uzAstoniekiem <- data.frame()
  x13_uzDesmitniekiem <- data.frame()
  check_rows <- 0
  
  for (r in seq(1, nrow(x), by = 13)) {

    x13 <- x[r:(r+12),]
    x13 <- x13[order(x13$period, x13$PS_code, x13$DN_code, x13$NM_code, x13$NDZ_sanemsanas_datums), ]
    
    if (sum(x13$end == "2") == 7) {
      if ((x13$end[1] == "2") &&(((x13$end[2] == "2" && x13$start[3] == "1") && (x13$NDZ_sanemsanas_datums[2] == x13$NDZ_sanemsanas_datums[3])) || (x13$start[2] == "1" && x13$end[3] == "2"))) {
        x13_uzTrijniekiem <- rbind(x13_uzTrijniekiem, x13[1: 3, ])
        x13_uzDesmitniekiem <- rbind(x13_uzDesmitniekiem, x13[4:13, ])
        check_rows <- check_rows + 13
      } else {
        stop("13-nieku tabulas pārdalei trūkst izstrādes koda. Rindas: ", r, " līdz ", r + 12, "\n")
      }
    } else if (sum(x13$start == "1") == 7) {
      if ((x13$start[1] == "1" && x13$end[2] == "2" && x13$start[3] == "1") && (x13$NDZ_sanemsanas_datums[1] <= x13$NDZ_sanemsanas_datums[2] && x13$NDZ_sanemsanas_datums[2] != x13$NDZ_sanemsanas_datums[3])) {
        if ((x13$start[11] == "1" && x13$end[12] == "2" && x13$start[13] == "1") && (x13$NDZ_sanemsanas_datums[12] != x13$NDZ_sanemsanas_datums[13])) {
          x13_uzTrijniekiem <- rbind(x13_uzTrijniekiem, x13[11:13, ])
          x13_uzDesmitniekiem <- rbind(x13_uzDesmitniekiem, x13[1:10, ])
          check_rows <- check_rows + 13
        } else if ((x13$start[9] == x13$start[10] && x13$end[11] == x13$end[13]) && (x13$NDZ_sanemsanas_datums[10] != x13$NDZ_sanemsanas_datums[11])) {
          x13_uzCetriniekiem <- rbind(x13_uzCetriniekiem, x13[10:13, ])
          x13_uzAstoniekiem <- rbind(x13_uzAstoniekiem, x13[1:8, ])
          check_rows <- check_rows + 13
        } else if (all(x13$start[c(1, 3, 5, 7, 9, 11, 13)] == "1") && 
                   all(x13$end[c(2, 4, 6, 8, 10, 12)] == "2") && 
                   all(diff(x13$NDZ_sanemsanas_datums[1:12]) != 0) &&
                   x13$NDZ_sanemsanas_datums[12] == x13$NDZ_sanemsanas_datums[13]) {
          x13_uzVieniniekiem <- rbind(x13_uzVieniniekiem, x13[13, ])
          x13_uzCetriniekiem <- rbind(x13_uzCetriniekiem, x13[1:4, ])
          x13_uzAstoniekiem <- rbind(x13_uzAstoniekiem, x13[5:12, ])
          check_rows <- check_rows + 13
        } else {
          stop("13-nieku tabulas pārdalei trūkst izstrādes koda. Rindas:", r, " līdz ", r + 12, "\n")
        }
      } else if (x13$start[11] == "1" && x13$end[12] == "2" && x13$start[13] == "1" &&
                        all(diff(x13$NDZ_sanemsanas_datums[11:13]) != 0)) { 
          x13_uzTrijniekiem <- rbind(x13_uzTrijniekiem, x13[11:13, ])
          x13_uzDesmitniekiem <- rbind(x13_uzDesmitniekiem, x13[1:10, ])
          check_rows <- check_rows + 13
      } else if (all(x13$start[c(2, 4, 6, 8, 9, 12, 13)] == "1") && 
                 all(x13$end[c(1, 3, 5, 7, 10, 11)] == "2") && 
                 all(sapply(seq(1, 8, by = 2), function(i) all(diff(x13$NDZ_sanemsanas_datums[i:i+1]) == 0))) &&
                 all(diff(x13$NDZ_sanemsanas_datums[11:12]) == 0) &&
                 x13$NDZ_sanemsanas_datums[12] != x13$NDZ_sanemsanas_datums[13]) {
        x13_uzVieniniekiem <- rbind(x13_uzVieniniekiem, x13[13, ])
        x13_uzCetriniekiem <- rbind(x13_uzCetriniekiem, x13[1:4, ])
        x13_uzAstoniekiem <- rbind(x13_uzAstoniekiem, x13[5:12, ])
        check_rows <- check_rows + 13
      } else {
        stop(cat("13-nieku tabulas pārdalei trūkst izstrādes koda. Rindas:",r, "līdz", r + 12, "\n"))
    }} else {
      stop(cat("13-nieku tabulas pārdalei trūkst izstrādes koda. Rindas:",r, "līdz", r + 12, "\n"))
    }
  }
  
  #2 PĀRBAUDE: Vai rindu skaits no 13-niekiem atvasinātajās tabulās sakrīt ar rindām izejas tabulā x.
  if (check_rows == nrow(x)) {
    cat("PĀRBAUDE IZIETA: Apakštabulu rindu summa sakrīt ar izejošo 13-nieku tabulu.\n")
    rm(x, x13, r, check_rows)
  } else {
    stop("ERROR: Apakštabulu rindu summa NESAKRĪT ar izejošo 13-nieku tabulu.\n")
  }
  
  #3 Apakštabulu x13_uzVieniniekiem sūta caur processingOnes().
  if (nrow(x13_uzVieniniekiem) > 0) {
    x13_uzVieniniekiem <- x13_uzVieniniekiem[order(x13_uzVieniniekiem$PS_code, x13_uzVieniniekiem$NM_code, x13_uzVieniniekiem$NDZ_sanemsanas_datums, x13_uzVieniniekiem$start),]
    cat("No 17-niekiem atvasinātā tabula x13_uzVieniniekiem pārsūtīta uz processingOnes() un tad uz tempNDZ, ko būvējam.\n")
    sendTo_tempNDZ(processingOnes(x13_uzVieniniekiem, o))
  } else {
    cat("Tabula x13_uzVieniniekiem ir tukša.\n")
  }
  
  rm(x13_uzVieniniekiem)
  
  #4 Apakštabulu x13_uzTrijniekiem sūta caur processingThrees().
  if (nrow(x13_uzTrijniekiem) > 0) {
    x13_uzTrijniekiem <- x13_uzTrijniekiem[order(x13_uzTrijniekiem$PS_code, x13_uzTrijniekiem$NM_code, x13_uzTrijniekiem$NDZ_sanemsanas_datums),]
    cat("No 13-niekiem atvasinātā tabula x13_uzTrijniekiem pārsūtīta uz processingThrees() un tad caur to uz tempNDZ, ko būvējam.\n")
    processingThrees(x13_uzTrijniekiem, o)
  } else {
    cat("Tabula x13_uzTrijniekiem ir tukša.\n")
  }
  
  rm(x13_uzTrijniekiem)
  
  #5 Apakštabulu x13_uzCetriniekiem sūta caur processingFours().
  if (nrow(x13_uzCetriniekiem) > 0) {
    x13_uzCetriniekiem <- x13_uzCetriniekiem[order(x13_uzCetriniekiem$PS_code, x13_uzCetriniekiem$NM_code, x13_uzCetriniekiem$NDZ_sanemsanas_datums, x13_uzCetriniekiem$start),]
    cat("No 13-niekiem atvasinātā tabula x13_uzCetriniekiem pārsūtīta uz processingFours() un tad caur to uz tempNDZ, ko būvējam.\n")
    processingFours(x13_uzCetriniekiem, o)
  } else {
    cat("Tabula x13_uzCetriniekiem ir tukša.\n")
  }
  
  rm(x13_uzCetriniekiem)
  
  #6 Apakštabulu x13_uzAstoniekiem sūta caur processingFours().
  if (nrow(x13_uzAstoniekiem) > 0) {
    x13_uzAstoniekiem <- x13_uzAstoniekiem[order(x13_uzAstoniekiem$PS_code, x13_uzAstoniekiem$NM_code, x13_uzAstoniekiem$NDZ_sanemsanas_datums, x13_uzAstoniekiem$start),]
    cat("No 13-niekiem atvasinātā tabula x13_uzAstoniekiem pārsūtīta uz processingEights() un tad caur to uz tempNDZ, ko būvējam.\n")
    processingEights(x13_uzAstoniekiem, o)
  } else {
    cat("Tabula x13_uzAstoniekiem ir tukša.\n")
  }
  
  rm(x13_uzAstoniekiem)
  
  #7 Apakštabulu x13_uzDesmitniekiem sūta caur processingTens().
  if (nrow(x13_uzDesmitniekiem) > 0) {
    x13_uzDesmitniekiem <-
      x13_uzDesmitniekiem[order(
        x13_uzDesmitniekiem$PS_code,
        x13_uzDesmitniekiem$NM_code,
        x13_uzDesmitniekiem$NDZ_sanemsanas_datums
      ),]
    cat(
      "No 13-niekiem atvasinātā tabula x13_uzDesmitniekiem pārsūtīta uz processingTens un tad uz tempNDZ, ko būvējam.\n"
    )
    processingTens(x13_uzDesmitniekiem, o)
  } else {
    cat("Tabula x13_uzDesmitniekiem ir tukša.\n")
  }
  rm(x13_uzDesmitniekiem)
}

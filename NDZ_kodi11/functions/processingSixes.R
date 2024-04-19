processingSixes <- function(x, o) {

  x <- x[order(x$PS_code, x$DN_code, x$NM_code, x$NDZ_sanemsanas_datums, x$zinkod), ]
  
  x6_uzVieniniekiem <- data.frame()
  x6_uzDivniekiem <- data.frame()
  x6_uzTris <- data.frame()
  x6_uzCetri <- data.frame()
  x6_uzPieciniekiem <- data.frame()
  check_rows <- 0
  
  for (r in seq(1, nrow(x), by = 6)) {
  
      x6 <- x[r:(r+5),]
      x6 <- x6[order(x6$PS_code, x6$DN_code, x6$NM_code, x6$NDZ_sanemsanas_datums), ]
      
    if (sum(x6$start == "1") == 6) {
      x_vieninieki <- codes_match(x6)
      x6_uzVieniniekiem <- rbind(x6_uzVieniniekiem, x_vieninieki)
      rm(x_vieninieki)
    } else if ((sum(x6$start == "1") == 3) &&
               (x6$sak_darbu[x6$start == "1"][1] <= x6$beidz_darbu[x6$end == "2"][1]) &&
               (x6$sak_darbu[x6$start == "1"][2] <= x6$beidz_darbu[x6$end == "2"][2]) &&
               (x6$sak_darbu[x6$start == "1"][3] <= x6$beidz_darbu[x6$end == "2"][3])) {
      x6_uzDivniekiem <- rbind(x6_uzDivniekiem, x6)
    } else if((x6$end[1] == "2" && x6$start[2] == "1") && (x6$NDZ_sanemsanas_datums[1] != x6$NDZ_sanemsanas_datums[2])) {
      x6_uzVieniniekiem <- rbind(x6_uzVieniniekiem, x6[1, ])
      x6_uzPieciniekiem <- rbind(x6_uzPieciniekiem, x6[2:6, ])
    } else if (x6$start[1] == "1" && x6$end[2] == "2" && x6$start[3] == "1" && x6$end[4] == "2" && x6$start[5] == x6$start[6] && x6$NDZ_sanemsanas_datums[1] != x6$NDZ_sanemsanas_datums[2] && x6$NDZ_sanemsanas_datums[3] != x6$NDZ_sanemsanas_datums[4] && x6$NDZ_sanemsanas_datums[4] != x6$NDZ_sanemsanas_datums[5] && x6$NDZ_sanemsanas_datums[5] != x6$NDZ_sanemsanas_datums[6]) {
      x6_uzDivniekiem <- rbind(x6_uzDivniekiem, x6[1:4, ])
      x6_uzVieniniekiem <- rbind(x6_uzVieniniekiem, x6[6, ])
    } else if (x6$end[1] == "2" && x6$end[2] == x6$end[1] && x6$start[3] == "1" && x6$NDZ_sanemsanas_datums[2] == x6$NDZ_sanemsanas_datums[3] && x6$start[4] == "1" && x6$end[5] == "2" && x6$start[6] == "1" && !(any(diff(x6$NDZ_sanemsanas_datums[3:6]) == 0))) {
      x6_uzVieniniekiem <- rbind(x6_uzVieniniekiem, x6[c(1, 6), ])
      x6_uzDivniekiem <- rbind(x6_uzDivniekiem, x6[2:5, ])
    } else if ((x6$NDZ_sanemsanas_datums[1] == x6$NDZ_sanemsanas_datums[2] && x6$NDZ_sanemsanas_datums[3] != x6$NDZ_sanemsanas_datums[4] && x6$start[1] != x6$start[2] && x6$end[3] != "2") || (!(any(diff(x6$NDZ_sanemsanas_datums) == 0)) && x6$start[1] == "1" && x6$end[2] == "2")) {
      x6_uzDivniekiem <- rbind(x6_uzDivniekiem, x6[1:2, ])
      x6_uzCetri <- rbind(x6_uzCetri, x6[3:6, ])
    } else if (x6$NDZ_sanemsanas_datums[1] == x6$NDZ_sanemsanas_datums[2] && x6$NDZ_sanemsanas_datums[3] != x6$NDZ_sanemsanas_datums[4] && x6$start[1] != x6$start[2] && x6$end[3] == "2") {
      x6_uzDivniekiem <- rbind(x6_uzDivniekiem, x6[2:3, ])
      x6_uzTris <- rbind(x6_uzTris, x6[4:6, ])
    } else if (all(x6$start[c(2,4,6)] == "1") && 
               all(sapply(seq(1,4, by=2), function(i) all(diff(x6$NDZ_sanemsanas_datums[i:i+1]) == 0))) &&
               x6$NDZ_sanemsanas_datums[5] != x6$NDZ_sanemsanas_datums[6] &&
               x6$PS_code[1] == "-------" && x6$NM_code[1] == "----------") {
      
      x6_uzVieniniekiem <- rbind(x6_uzVieniniekiem, x6[1, ])
      x6_uzPieciniekiem <- rbind(x6_uzPieciniekiem, x6[-1, ])
    } else {
      stop("Sešinieku tabula nepārdalījās.\n",
          "Nav apstrādes koda sešinieku apakštabulai!\n",
          "Problēmu skatīt rindā",
          r,
          ", lietojot", paste0("x[x$PS_code == x$PS_code[",
          r,
          "], ]"), "\n"
        )
    }
    check_rows <- check_rows + 6
  }
  
  #Pārbaude
  if(check_rows == nrow(x)) {
    cat("PĀRBAUDE IZIETA:\n
      Rindu summa no sešiniekiem atvasinātajās tabulās sakrīt ar rindu skaitu oriģinālajā tabulā NDZ_6.\n")
  } else {
    stop(cat("PĀRBAUDE NAV IZIETA.\n
           Rindu summa no sešiniekiem atvasinātajās tabulās NESAKRĪT ar rindu skaitu oriģinālajā tabulā NDZ_6.\n"))
  }
  
  rm(x, r, x6, check_rows)
  
  #3) Apakštabulu x6_uzVieniniekiem apstrādā caur processingOnes function.
  if(nrow(x6_uzVieniniekiem) > 0) {
    x6_uzVieniniekiem <- x6_uzVieniniekiem[order(x6_uzVieniniekiem$PS_code, x6_uzVieniniekiem$NM_code, x6_uzVieniniekiem$NDZ_sanemsanas_datums), ]
    sendTo_tempNDZ(processingOnes(x6_uzVieniniekiem, o))
  } else {
    cat("Tabula x6_uzVieniniekiem ir tukša.\n")
  }
  rm(x6_uzVieniniekiem)
  
  #4) Apakštabulu x6_uzDivniekiem sūta caur processingTwoes.
  if(nrow(x6_uzDivniekiem) > 0) {
    x6_uzDivniekiem <- x6_uzDivniekiem[order(x6_uzDivniekiem$PS_code, x6_uzDivniekiem$NM_code, x6_uzDivniekiem$NDZ_sanemsanas_datums), ]
    processingTwoes(x6_uzDivniekiem, o)
  } else {
    cat("Tabula x6_uzDivniekiem ir tukša.\n")
  }
  
  rm(x6_uzDivniekiem)
  
  #5) Apakštabulu x6_uzTris sūta caur processingThrees.
  if(nrow(x6_uzTris) > 0) {
    x6_uzTris <- x6_uzTris[order(x6_uzTris$PS_code, x6_uzTris$NM_code, x6_uzTris$NDZ_sanemsanas_datums), ]
    processingThrees(x6_uzTris, o)
  } else {
    cat("Tabula x6_uzTris ir tukša.")
  }
  
  rm(x6_uzTris)

  #6) Apakštabulu x6_uzCetri sūta caur processingFours.
  if(nrow(x6_uzCetri) > 0) {
    x6_uzCetri <- x6_uzCetri[order(x6_uzCetri$PS_code, x6_uzCetri$NM_code, x6_uzCetri$NDZ_sanemsanas_datums), ]
    processingFours(x6_uzCetri, o)
  } else {
    cat("Tabula x6_uzCetri ir tukša.\n")
  }
  
  rm(x6_uzCetri)
  
  #7) Apakštabulu x6_uzPieciniekiem sūta caur processingFives.
  if(nrow(x6_uzPieciniekiem) > 0) {
    x6_uzPieciniekiem <- x6_uzPieciniekiem[order(x6_uzPieciniekiem$PS_code, x6_uzPieciniekiem$NM_code, x6_uzPieciniekiem$NDZ_sanemsanas_datums), ]
    processingFives(x6_uzPieciniekiem, o)
  } else {
    cat("Tabula x6_uzPieciniekiem ir tukša.\n")
  }
  
  rm(x6_uzPieciniekiem)
}

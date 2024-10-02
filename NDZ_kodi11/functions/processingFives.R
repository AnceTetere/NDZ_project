processingFives <- function(x, o) {
  x <- x[order(x$PS_code, x$DN_code, x$NM_code, x$NDZ_sanemsanas_datums, x$zinkod), ]

  x5_uzVieniniekiem <- data.frame()
  x5_uzDivniekiem <- data.frame()
  x5_uzCetriniekiem <- data.frame()
  
  fncResult <- function(result) {
    if (exists("result")) {
      x5_uzVieniniekiem <- rbind(x5_uzVieniniekiem, result$x5_uzVieniniekiem)
      x5_uzDivniekiem <- rbind(x5_uzDivniekiem, result$x5_uzDivniekiem)
      x5_uzCetriniekiem <- rbind(x5_uzCetriniekiem, result$x5_uzCetriniekiem)
    }
    rm(result)
  }

  for (r in seq(1, nrow(x), by = 5)) {
    x5 <- x[r:(r+4), ]
    x5 <- x5[order(x5$PS_code, x5$DN_code, x5$NM_code, x5$NDZ_sanemsanas_datums, x5$zinkod), ]

    if (sum(x5$sak_beidz == "1") == 5) {
      x_vieninieki <- codes_match(x5)
      x5_uzVieniniekiem <- rbind(x5_uzVieniniekiem, x_vieninieki)
      rm(x_vieninieki)
    } else if (sum(x5$sak_beidz == "1") == 4){
      if (sum(x5$NDZ_sanemsanas_datums[x5$sak_beidz == "1"] <= x5$NDZ_sanemsanas_datums[x5$sak_beidz == "2"]) == 4) {
        logVec_forsak_beidz <- ifelse(is.na(x5$NDZ_sanemsanas_datums == x5$NDZ_sanemsanas_datums[x5$sak_beidz == "1"][(abs(as.numeric(difftime(x5$NDZ_sanemsanas_datums[x5$sak_beidz == "1"], x5$NDZ_sanemsanas_datums[x5$sak_beidz == "2"], units = "days"))) == min(abs(as.numeric(difftime(x5$NDZ_sanemsanas_datums[x5$sak_beidz == "1"], x5$NDZ_sanemsanas_datums[x5$sak_beidz == "2"], units = "days")))))]), FALSE, x5$NDZ_sanemsanas_datums == x5$NDZ_sanemsanas_datums[x5$sak_beidz == "1"][(abs(as.numeric(difftime(x5$NDZ_sanemsanas_datums[x5$sak_beidz == "1"], x5$NDZ_sanemsanas_datums[x5$sak_beidz == "2"], units = "days"))) == min(abs(as.numeric(difftime(x5$NDZ_sanemsanas_datums[x5$sak_beidz == "1"], x5$NDZ_sanemsanas_datums[x5$sak_beidz == "2"], units = "days")))))])
        x5_uzDivniekiem <- rbind(x5_uzDivniekiem, x5[logVec_forsak_beidz, ], x5[x5$sak_beidz == "2", ]) 
        rm(logVec_forsak_beidz)
      } else if (all(x5$sak_beidz[1:5] == c("1", "1", "2", "1", "1")) && x5$NDZ_sanemsanas_datums[1] != x5$NDZ_sanemsanas_datums[2]) {
        x5_uzDivniekiem <- rbind(x5_uzDivniekiem, x5[2:3, ])
        x5_uzVieniniekiem <- rbind(x5_uzVieniniekiem, x5[5, ])
      } else if (all(x5$sak_beidz[c(1, 3:5)] == "1") && all(diff(x5$NDZ_sanemsanas_datums[c(1, 3:5)]) != 0) && all(diff(x5$NDZ_sanemsanas_datums[2:3]) == 0)) {
        x5_uzDivniekiem <- rbind(x5_uzDivniekiem, x5[1:2, ])
        x5_uzVieniniekiem <- rbind(x5_uzVieniniekiem, x5[5, ])
      } else {
        stop("ERROR: Šis gadījums funkcijā processingFives nav izstrādāts. Rinda: ", r, ".\n")
      }
    } else if ((all(x5$sak_beidz[1:2] == c("2", "1")) && x5$NDZ_sanemsanas_datums[1] == x5$NDZ_sanemsanas_datums[2]) || (x5$sak_beidz[1] == "1" && x5$sak_beidz[2] == "2" && x5$NDZ_sanemsanas_datums[1] <= x5$NDZ_sanemsanas_datums[2]) && ((x5$sak_beidz[3] == "1" && x5$sak_beidz[4] == "2" && x5$NDZ_sanemsanas_datums[3] <= x5$NDZ_sanemsanas_datums[4] && x5$NDZ_sanemsanas_datums[2] != x5$NDZ_sanemsanas_datums[3])||(x5$sak_beidz[3] == "2" && x5$sak_beidz[4] == "1" && x5$NDZ_sanemsanas_datums[3] == x5$NDZ_sanemsanas_datums[4] && x5$NDZ_sanemsanas_datums[2] != x5$NDZ_sanemsanas_datums[3])) && (x5$sak_beidz[5] == "1" && x5$NDZ_sanemsanas_datums[4] != x5$NDZ_sanemsanas_datums[5])) {
      x5_uzCetriniekiem <- rbind(x5_uzCetriniekiem, x5[1:4, ])
      x5_uzVieniniekiem <- rbind(x5_uzVieniniekiem, x5[5, ])
    } else if ((x5$sak_beidz[1] == "1" && x5$sak_beidz[2] == "1" && x5$sak_beidz[3] == "2" && x5$sak_beidz[4] == "1" && x5$sak_beidz[5] == "2") && (x5$NDZ_sanemsanas_datums[3] != x5$NDZ_sanemsanas_datums[4])) {
      x5_uzCetriniekiem <- rbind(x5_uzCetriniekiem, x5[-1, ])
    } else if ((x5$sak_beidz[1] == "2" && x5$sak_beidz[2] == "1" && x5$NDZ_sanemsanas_datums[1] == x5$NDZ_sanemsanas_datums[2]) && (x5$sak_beidz[3] == "2" && x5$NDZ_sanemsanas_datums[2] != x5$NDZ_sanemsanas_datums[3]) && (x5$sak_beidz[4] == "1" && x5$NDZ_sanemsanas_datums[3] != x5$NDZ_sanemsanas_datums[4]) && (x5$sak_beidz[5] == "1" && x5$NDZ_sanemsanas_datums[4] != x5$NDZ_sanemsanas_datums[5]) && ("50" %in% x5$zinkod)) {
      x5_uzVieniniekiem <- rbind(x5_uzVieniniekiem, x5[c(1, 5), ])
      x5_uzDivniekiem <- rbind(x5_uzDivniekiem, x5[2:3, ])
    } else if ((x5$sak_beidz[1] == "2" && (x5$sak_beidz[2] == "1"|| x5$sak_beidz[2] == "2") && x5$NDZ_sanemsanas_datums[1] != x5$NDZ_sanemsanas_datums[2]) && 
               ((x5$sak_beidz[2] == "1"|| x5$sak_beidz[2] == "2") && (x5$sak_beidz[3] == "2" || x5$sak_beidz[3] == "1") && x5$sak_beidz[4] == "2" && x5$NDZ_sanemsanas_datums[2] == x5$NDZ_sanemsanas_datums[3] && x5$NDZ_sanemsanas_datums[3] != x5$NDZ_sanemsanas_datums[4]) && 
               (x5$sak_beidz[5] == "1" && x5$NDZ_sanemsanas_datums[4] != x5$NDZ_sanemsanas_datums[5])) {
      x5_uzVieniniekiem <- rbind(x5_uzVieniniekiem, x5[c(1, 5), ])
      ifelse(x5$sak_beidz[2] == "1", s <- 2, s <- 3)
      x5_uzDivniekiem <- rbind(x5_uzDivniekiem, x5[c(s, 4), ])
      rm(s)
    } else if ((x5$sak_beidz[1] == "2" && x5$sak_beidz[2] == "1" && x5$sak_beidz[3] == "2" && x5$sak_beidz[4] == "2" && x5$sak_beidz[5] == "1" && any(diff(x5$NDZ_sanemsanas_datums[1:4]) != 0) && x5$NDZ_sanemsanas_datums[4] == x5$NDZ_sanemsanas_datums[5]) || (x5$sak_beidz[1] == "2" && x5$sak_beidz[2] == "1" && x5$sak_beidz[3] == "2" && x5$NDZ_sanemsanas_datums[1] != x5$NDZ_sanemsanas_datums[2] && x5$NDZ_sanemsanas_datums[2] != x5$NDZ_sanemsanas_datums[3]) && (x5$sak_beidz[4] == "1" && x5$sak_beidz[5] == "2" && x5$NDZ_sanemsanas_datums[3] != x5$NDZ_sanemsanas_datums[4] && x5$NDZ_sanemsanas_datums[4] != x5$NDZ_sanemsanas_datums[5]) || (x5$sak_beidz[1] == "2" && x5$sak_beidz[2] == "2" && x5$NDZ_sanemsanas_datums[1] != x5$NDZ_sanemsanas_datums[2] && x5$sak_beidz[3] == x5$sak_beidz[4] && x5$NDZ_sanemsanas_datums[2] == x5$NDZ_sanemsanas_datums[3] && x5$sak_beidz[5] == "2" && x5$NDZ_sanemsanas_datums[4] != x5$NDZ_sanemsanas_datums[5]) || (x5$sak_beidz[1] == "2" && x5$sak_beidz[2] == "2" && x5$sak_beidz[3] == "1" && x5$sak_beidz[4] == "2" && x5$sak_beidz[5] == "1" && x5$NDZ_sanemsanas_datums[1] != x5$NDZ_sanemsanas_datums[2] && x5$NDZ_sanemsanas_datums[2] == x5$NDZ_sanemsanas_datums[3] && x5$NDZ_sanemsanas_datums[4] == x5$NDZ_sanemsanas_datums[5])) {
      x5_uzVieniniekiem <- rbind(x5_uzVieniniekiem, x5[1, ]) 
      x5_uzCetriniekiem <- rbind(x5_uzCetriniekiem, x5[-1, ])
  
  } else if (sum(x5$sak_beidz == "1") == 3) {
    fncResult(processingFives_s3(x5))
  } else if (sum(x5$sak_beidz == "1") == 2) {
    fncResult(processingFives_s2(x5))
  } else {stop("processingFives: Tabula nepārdalījās; rinda: ", r, ".\n")}
  }
  
  rm(x, r, x5, fncResult)
  
#1 Apakštabulu x5_uzVieniniekiem apstrādā caur funkciju processingOnes().
  if(nrow(x5_uzVieniniekiem) > 0) {
    x5_uzVieniniekiem <- x5_uzVieniniekiem[order(x5_uzVieniniekiem$PS_code, x5_uzVieniniekiem$NM_code, x5_uzVieniniekiem$NDZ_sanemsanas_datums, x5_uzVieniniekiem$zinkod), ]
    sendTo_tempNDZ(processingOnes(x5_uzVieniniekiem, o))
  } else { 
    cat("Tabula x5_uzVieniniekiem ir tukša.\n")
  }
  
  rm(x5_uzVieniniekiem)
  
#2 Apakštabulu x5_uzDivniekiem apstrādā caur funkciju processingTwoes().
  if(nrow(x5_uzDivniekiem) > 0) {
    x5_uzDivniekiem <- x5_uzDivniekiem[order(x5_uzDivniekiem$PS_code, x5_uzDivniekiem$NM_code, x5_uzDivniekiem$NDZ_sanemsanas_datums, x5_uzDivniekiem$zinkod), ]
    processingTwoes(x5_uzDivniekiem, o)
  } else { 
    cat("Tabula x5_uzDivniekiem ir tukša.\n")
  }
  
  rm(x5_uzDivniekiem)
  
#3 Apakštabulu x5_uzCetriniekiem sūta caur funkciju processingFours().
if(nrow(x5_uzCetriniekiem > 0)) {
  x5_uzCetriniekiem <- x5_uzCetriniekiem[order(x5_uzCetriniekiem$PS_code, x5_uzCetriniekiem$NM_code, x5_uzCetriniekiem$NDZ_sanemsanas_datums, x5_uzCetriniekiem$zinkod), ]
  processingFours(x5_uzCetriniekiem, o)
} else {
  cat("Tabula x5_uzCetriniekiem ir tukša.\n")
}

  rm(x5_uzCetriniekiem)
}

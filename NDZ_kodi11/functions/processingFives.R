processingFives <- function(x, o) {
  x <- x[order(x$PS_code, x$DN_code, x$processingFives <- function(x, o) {
  x <- x[order(x$PS_code, x$DN_code, x$NM_code, x$NDZ_sanemsanas_datums), ]

  x5_uzVieniniekiem <- data.frame()
  x5_uzDivniekiem <- data.frame()
  x5_uzCetriniekiem <- data.frame()

  for (r in seq(1, nrow(x), by = 5)) {
    x5 <- x[r:(r+4), ]
    x5 <- x5[order(x5$PS_code, x5$DN_code, x5$NM_code, x5$NDZ_sanemsanas_datums), ]

    if (sum(x5$start == "1") == 5) {
      x_vieninieki <- codes_match(x5)
      x5_uzVieniniekiem <- rbind(x5_uzVieniniekiem, x_vieninieki)
      rm(x_vieninieki)
    } else if (sum(x5$start == "1") == 4){
      if (sum(x5$sak_darbu[x5$start == "1"] <= x5$beidz_darbu[x5$end == "2"]) == 4) {
        logVec_forStart <- ifelse(is.na(x5$sak_darbu == x5$sak_darbu[x5$start == "1"][(abs(as.numeric(difftime(x5$sak_darbu[x5$start == "1"], x5$beidz_darbu[x5$end == "2"], units = "days"))) == min(abs(as.numeric(difftime(x5$sak_darbu[x5$start == "1"], x5$beidz_darbu[x5$end == "2"], units = "days")))))]), FALSE, x5$sak_darbu == x5$sak_darbu[x5$start == "1"][(abs(as.numeric(difftime(x5$sak_darbu[x5$start == "1"], x5$beidz_darbu[x5$end == "2"], units = "days"))) == min(abs(as.numeric(difftime(x5$sak_darbu[x5$start == "1"], x5$beidz_darbu[x5$end == "2"], units = "days")))))])
        x5_uzDivniekiem <- rbind(x5_uzDivniekiem, x5[logVec_forStart, ], x5[x5$end == "2", ]) 
        rm(logVec_forStart)
      } else if (x5$start[1] == "1" && x5$start[2] == "1" && x5$end[3] == "2" && x5$start[4] == "1" && x5$start[5] == "1" && x5$NDZ_sanemsanas_datums[1] != x5$NDZ_sanemsanas_datums[2]) {
        x5_uzDivniekiem <- rbind(x5_uzDivniekiem, x5[2:3, ])
        x5_uzVieniniekiem <- rbind(x5_uzVieniniekiem, x5[5, ])
      } else {
        stop("ERROR: Šis gadījums funkcijā processingFives nav izstrādāts. Rinda: ", r, ".\n")
      }
    } else if (((x5$end[1] == "2" && x5$start[2] == "1" && x5$NDZ_sanemsanas_datums[1] == x5$NDZ_sanemsanas_datums[2]) || (x5$start[1] == "1" && x5$end[2] == "2" && x5$NDZ_sanemsanas_datums[1] <= x5$NDZ_sanemsanas_datums[2])) && ((x5$start[3] == "1" && x5$end[4] == "2" && x5$NDZ_sanemsanas_datums[3] <= x5$NDZ_sanemsanas_datums[4] && x5$NDZ_sanemsanas_datums[2] != x5$NDZ_sanemsanas_datums[3])||(x5$end[3] == "2" && x5$start[4] == "1" && x5$NDZ_sanemsanas_datums[3] == x5$NDZ_sanemsanas_datums[4] && x5$NDZ_sanemsanas_datums[2] != x5$NDZ_sanemsanas_datums[3])) && (x5$start[5] == "1" && x5$NDZ_sanemsanas_datums[4] != x5$NDZ_sanemsanas_datums[5])){
      x5_uzCetriniekiem <- rbind(x5_uzCetriniekiem, x5[1:4, ])
      x5_uzVieniniekiem <- rbind(x5_uzVieniniekiem, x5[5, ])
    } else if ((x5$start[1] == "1" && x5$start[2] == "1" && x5$end[3] == "2" && x5$start[4] == "1" && x5$end[5] == "2") && (x5$NDZ_sanemsanas_datums[3] != x5$NDZ_sanemsanas_datums[4])) {
      x5_uzCetriniekiem <- rbind(x5_uzCetriniekiem, x5[-1, ])
    } else if ((x5$end[1] == "2" && x5$start[2] == "1" && x5$NDZ_sanemsanas_datums[1] == x5$NDZ_sanemsanas_datums[2]) && (x5$end[3] == "2" && x5$NDZ_sanemsanas_datums[2] != x5$NDZ_sanemsanas_datums[3]) && (x5$start[4] == "1" && x5$NDZ_sanemsanas_datums[3] != x5$NDZ_sanemsanas_datums[4]) && (x5$start[5] == "1" && x5$NDZ_sanemsanas_datums[4] != x5$NDZ_sanemsanas_datums[5]) && ("50" %in% x5$zinkod)) {
      x5_uzVieniniekiem <- rbind(x5_uzVieniniekiem, x5[c(1, 5), ])
      x5_uzDivniekiem <- rbind(x5_uzDivniekiem, x5[2:3, ])
    } else if ((x5$end[1] == "2" && (x5$start[2] == "1"|| x5$end[2] == "2") && x5$NDZ_sanemsanas_datums[1] != x5$NDZ_sanemsanas_datums[2]) && 
               ((x5$start[2] == "1"|| x5$end[2] == "2") && (x5$end[3] == "2" || x5$start[3] == "1") && x5$end[4] == "2" && x5$NDZ_sanemsanas_datums[2] == x5$NDZ_sanemsanas_datums[3] && x5$NDZ_sanemsanas_datums[3] != x5$NDZ_sanemsanas_datums[4]) && 
               (x5$start[5] == "1" && x5$NDZ_sanemsanas_datums[4] != x5$NDZ_sanemsanas_datums[5])) {
      x5_uzVieniniekiem <- rbind(x5_uzVieniniekiem, x5[c(1, 5), ])
      ifelse(x5$start[2] == "1", s <- 2, s <- 3)
      x5_uzDivniekiem <- rbind(x5_uzDivniekiem, x5[c(s, 4), ])
      rm(s)
    } else if ((x5$end[1] == "2" && x5$start[2] == "1" && x5$end[3] == "2" && x5$end[4] == "2" && x5$start[5] == "1" && any(diff(x5$NDZ_sanemsanas_datums[1:4]) != 0) && x5$NDZ_sanemsanas_datums[4] == x5$NDZ_sanemsanas_datums[5]) || (x5$end[1] == "2" && x5$start[2] == "1" && x5$end[3] == "2" && x5$NDZ_sanemsanas_datums[1] != x5$NDZ_sanemsanas_datums[2] && x5$NDZ_sanemsanas_datums[2] != x5$NDZ_sanemsanas_datums[3]) && (x5$start[4] == "1" && x5$end[5] == "2" && x5$NDZ_sanemsanas_datums[3] != x5$NDZ_sanemsanas_datums[4] && x5$NDZ_sanemsanas_datums[4] != x5$NDZ_sanemsanas_datums[5]) || (x5$end[1] == "2" && x5$end[2] == "2" && x5$NDZ_sanemsanas_datums[1] != x5$NDZ_sanemsanas_datums[2] && x5$start[3] == x5$start[4] && x5$NDZ_sanemsanas_datums[2] == x5$NDZ_sanemsanas_datums[3] && x5$end[5] == "2" && x5$NDZ_sanemsanas_datums[4] != x5$NDZ_sanemsanas_datums[5]) || (x5$end[1] == "2" && x5$end[2] == "2" && x5$start[3] == "1" && x5$end[4] == "2" && x5$start[5] == "1" && x5$NDZ_sanemsanas_datums[1] != x5$NDZ_sanemsanas_datums[2] && x5$NDZ_sanemsanas_datums[2] == x5$NDZ_sanemsanas_datums[3] && x5$NDZ_sanemsanas_datums[4] == x5$NDZ_sanemsanas_datums[5])) {
      x5_uzVieniniekiem <- rbind(x5_uzVieniniekiem, x5[1, ]) 
      x5_uzCetriniekiem <- rbind(x5_uzCetriniekiem, x5[-1, ])
    } else if (sum(x5$start == "1") == 3) {
      if((x5$end[1] == "2" && x5$start[2] == "1" && x5$end[3] == "2" && x5$start[4] == "1" && x5$start[5] == "1") && (x5$NDZ_sanemsanas_datums[1] != x5$NDZ_sanemsanas_datums[2] && x5$NDZ_sanemsanas_datums[2] != x5$NDZ_sanemsanas_datums[3] && x5$NDZ_sanemsanas_datums[3] != x5$NDZ_sanemsanas_datums[4] && x5$NDZ_sanemsanas_datums[4] != x5$NDZ_sanemsanas_datums[5])) {
        x5_uzVieniniekiem <- rbind(x5_uzVieniniekiem, x5[c(1, 5), ])
        x5_uzDivniekiem <- rbind(x5_uzDivniekiem, x5[2:3, ])
      } else if ((x5$end[1] == "2" && x5$start[2] == "1" && x5$start[3] == "1" && x5$end[4] == "2" && x5$start[5] == "1") && (x5$NDZ_sanemsanas_datums[1] != x5$NDZ_sanemsanas_datums[2] && x5$NDZ_sanemsanas_datums[2] != x5$NDZ_sanemsanas_datums[3] && x5$NDZ_sanemsanas_datums[3] != x5$NDZ_sanemsanas_datums[4] && x5$NDZ_sanemsanas_datums[4] != x5$NDZ_sanemsanas_datums[5])) {
        x5_uzVieniniekiem <- rbind(x5_uzVieniniekiem, x5[c(1, 5), ])
        x5_uzDivniekiem <- rbind(x5_uzDivniekiem, x5[3:4, ])
      } else if (x5$end[1] == "2" && x5$start[2] == "1" && x5$start[3] == "1" && x5$start[4] == "1" && x5$end[5] == "2" && any(diff(x5$NDZ_sanemsanas_datums) != 0)) {
        x5_uzVieniniekiem <- rbind(x5_uzVieniniekiem, x5[1, ])
        x5_uzDivniekiem <- rbind(x5_uzDivniekiem, x5[4:5, ])
      } else if (x5$start[1] == "1" && x5$end[2] == "2" && x5$start[3] == "1" && x5$end[4] == "2" && x5$start[5] == "1" && any(diff(x5$NDZ_sanemsanas_datums[1:3]) != 0) && x5$NDZ_sanemsanas_datums[4] == x5$NDZ_sanemsanas_datums[5]) {
        x5_uzVieniniekiem <- rbind(x5_uzVieniniekiem, x5[5, ])
        x5_uzDivniekiem <- rbind(x5_uzDivniekiem, x5[1:4, ])
      } else if (x5$end[1] == "2" && x5$start[2] == "1" && x5$start[3] == "1" && x5$end[4] == "2" && x5$start[5] == "1" && x5$NDZ_sanemsanas_datums[1] == x5$NDZ_sanemsanas_datums[2]) {
        x5_uzVieniniekiem <- rbind(x5_uzVieniniekiem, x5[5, ])
        x5_uzDivniekiem <- rbind(x5_uzDivniekiem, x5[1:4, ])
      } else if (all(x5$start[c(1,3,5)] == "1") && 
                 all(diff(x5$NDZ_sanemsanas_datums[2:3]) == 0) && 
                 all(diff(x5$NDZ_sanemsanas_datums[3:5]) != 0) && 
                 all(diff(x5$NDZ_sanemsanas_datums[1:2]) != 0) && 
                 x5$PS_code[1] == "__________" && x5$NM_code[1] == "___________") {
        # Šis ir tas unokālais gadījums, kluram man vajadzēja visu gadu, lai izlobītu, kas tur notiek, j
        #jo indivīds ik pārdienas ņem bezalgas atvaļinājumus tā, ka atvaļinājuma sākuma dienā atsāk darbu, bet citreiz nē.
        #Nebiju pārliecināta, ka šo var vispārināt.
        x5_uzVieniniekiem <- rbind(x5_uzVieniniekiem, x5[5, ])
        x5_uzCetriniekiem <- rbind(x5_uzCetriniekiem, x5[-5, ])
      } else if (all(x5$start[1:3] == "1") && 
                 all(diff(x5$NDZ_sanemsanas_datums[2:3]) == 0) && 
                 all(diff(x5$NDZ_sanemsanas_datums[1:2]) != 0) && 
                 all(diff(x5$NDZ_sanemsanas_datums[3:5]) != 0)) {
        x5_uzDivniekiem <- rbind(x5_uzDivniekiem, x5[c(3,5), ])
      } else if (all(x5$start[3:5] == "1") && 
           all(diff(x5$NDZ_sanemsanas_datums[1:3]) == 0) && 
           diff(x5$NDZ_sanemsanas_datums[4:5]) == 0 && diff(x5$NDZ_sanemsanas_datums[3:4]) != 0) {
        x5_uzVieniniekiem <- rbind(x5_uzVieniniekiem, x5[c(3,5), ])
      } else if (all(x5$start[3:5] == "1") && 
                 all(diff(x5$NDZ_sanemsanas_datums[1:3]) == 0) && 
                 all(diff(x5$NDZ_sanemsanas_datums[3:5]) != 0)) {
        x5_uzVieniniekiem <- rbind(x5_uzVieniniekiem, x5[c(1,5), ])
      } else {stop("ERROR: Piecinieku gadījumam iztrūkst apstrādes kods: ", paste0("rinda: ", r, "."), "\n")}
    } else if (sum(x5$start == "1") == 2) {
      
      if(x5$end[1] == x5$end[2] && x5$end[2] == "2" && x5$start[3] == x5$start[4] && x5$start[3] == "1" && x5$end[5] == "2" && any(diff(x5$NDZ_sanemsanas_datums) != 0)) {
        x5_uzVieniniekiem <- rbind(x5_uzVieniniekiem, x5[2, ])
        x5_uzDivniekiem <- rbind(x5_uzDivniekiem, x5[4:5, ])
      } else if (x5$start[1] == "1" && x5$end[2] == "2" && x5$end[3] == "2" && x5$start[4] == "1" && x5$end[5] == "2" && x5$NDZ_sanemsanas_datums[3] == x5$NDZ_sanemsanas_datums[4]) {
        x5_uzDivniekiem <- rbind(x5_uzDivniekiem, x5[-3, ])
      } else if (x5$end[1] == "2" && x5$start[2] == "1" && x5$end[3] == "2" && x5$start[4] == "1" && x5$end[5] == "2" && 
                 x5$NDZ_sanemsanas_datums[1] == x5$NDZ_sanemsanas_datums[2] && all(diff(x5$NDZ_sanemsanas_datums[2:5]) != 0) && x5$zinkod[3] == "26") {
        x5_uzVieniniekiem <- rbind(x5_uzVieniniekiem, x5[1, ])
        x5_uzCetriniekiem <- rbind(x5_uzCetriniekiem, x5[-1, ])
      } else if (x5$end[1] == "2" && x5$start[2] == "1" && x5$end[3] == "2" && x5$start[4] == "1" && x5$end[5] == "2" && 
                 x5$NDZ_sanemsanas_datums[3] == x5$NDZ_sanemsanas_datums[4] && all(diff(x5$NDZ_sanemsanas_datums[1:3]) != 0) && 
                 all(diff(x5$NDZ_sanemsanas_datums[4:5]) != 0) && x5$zinkod[3] == "26") {
        x5_uzVieniniekiem <- rbind(x5_uzVieniniekiem, x5[1, ])
        x5_uzCetriniekiem <- rbind(x5_uzCetriniekiem, x5[-1, ])
      } else if (all(x5$start[c(3,5)] == "1") &&  
                 diff(x5$NDZ_sanemsanas_datums[1:2]) == diff(x5$NDZ_sanemsanas_datums[4:5]) &&
                 all(diff(x5$NDZ_sanemsanas_datums[2:4]) != 0) && x5$PS_code[1] == '__________' && x5$NM_code[1] == '___________') {
        x5_uzVieniniekiem <- rbind(x5_uzVieniniekiem, x5[c(1,3), ])
      } else if (all(x5$start[c(1,3)] == "1") &&  
                 all(diff(x5$NDZ_sanemsanas_datums) != 0)) {
        x5_uzDivniekiem <- rbind(x5_uzDivniekiem, x5[c(1:3,5), ])
      } else {
        stop(cat("ERROR: Šis gadījums funkcijā processingFives nav izstrādāts. Rinda: ", r, ".\n"))
      }
    }  else {
      stop(cat("ERROR: Piecinieku tabula nepārdalījās. Izejas tabulas x rinda: ", r, ".\n"))
    }
  }
  cat("SKAIDROJUMS: Piecinieku tabula sadalīta tabulās x5_uzVieniniekiem:", nrow(x5_uzVieniniekiem),
      "rindas; x5_uzDivniekiem:", nrow(x5_uzDivniekiem),
      "rindas; un x5_uzCetriniekiem: ", nrow(x5_uzCetriniekiem), "rindas.\n
    Tabulu x5_uzVieniniekiem tālāk pārstrādāt caur processingOnes.\n
    Tabulu x5_uzDivniekiem tālāk pārstrādāt caur processingTwoes.\n
    Tabulu x5_uzCetriniekiem tālāk pārstrādāt caur processingFours.\n ")
  
  rm(x, r, x5)
  
  #3) Apakštabulu x5_uzVieniniekiem apstrādā caur processingOnes function.
  if(nrow(x5_uzVieniniekiem) > 0) {
    x5_uzVieniniekiem <- x5_uzVieniniekiem[order(x5_uzVieniniekiem$PS_code, x5_uzVieniniekiem$NM_code, x5_uzVieniniekiem$NDZ_sanemsanas_datums, x5_uzVieniniekiem$zinkod), ]
    sendTo_tempNDZ(processingOnes(x5_uzVieniniekiem, o))
  } else { 
    cat("Tabula x5_uzVieniniekiem ir tukša.\n")
  }
  
  rm(x5_uzVieniniekiem)
  
  #4) Apakštabulu x5_uzDivniekiem apstrādā caur processingTwoes function.
  if(nrow(x5_uzDivniekiem) > 0) {
    x5_uzDivniekiem <- x5_uzDivniekiem[order(x5_uzDivniekiem$PS_code, x5_uzDivniekiem$NM_code, x5_uzDivniekiem$NDZ_sanemsanas_datums, x5_uzDivniekiem$zinkod), ]
    processingTwoes(x5_uzDivniekiem, o)
  } else { 
    cat("Tabula x5_uzDivniekiem ir tukša.\n")
  }
  
  rm(x5_uzDivniekiem)
  
  #5) Apakštabulu x5_uzCetriniekiem sūta caur processingFours function.
  if(nrow(x5_uzCetriniekiem > 0)) {
    x5_uzCetriniekiem <- x5_uzCetriniekiem[order(x5_uzCetriniekiem$PS_code, x5_uzCetriniekiem$NM_code, x5_uzCetriniekiem$NDZ_sanemsanas_datums, x5_uzCetriniekiem$zinkod), ]
    processingFours(x5_uzCetriniekiem, o)
  } else {
    cat("Tabula x5_uzCetriniekiem ir tukša.\n")
  }

  rm(x5_uzCetriniekiem)
}
, x$NDZ_sanemsanas_datums), ]

  x5_uzVieniniekiem <- data.frame()
  x5_uzDivniekiem <- data.frame()
  x5_uzCetriniekiem <- data.frame()

  for (r in seq(1, nrow(x), by = 5)) {
    x5 <- x[r:(r+4), ]
    x5 <- x5[order(x5$PS_code, x5$DN_code, x5$NM_code, x5$NDZ_sanemsanas_datums, x5$start, x5$end), ]

    if (sum(x5$start == "1") == 5) {
      x_vieninieki <- codes_match(x5)
      x5_uzVieniniekiem <- rbind(x5_uzVieniniekiem, x_vieninieki)
      rm(x_vieninieki)
    } else if (sum(x5$start == "1") == 4){
      if (sum(x5$sak_darbu[x5$start == "1"] <= x5$beidz_darbu[x5$end == "2"]) == 4) {
        logVec_forStart <- ifelse(is.na(x5$sak_darbu == x5$sak_darbu[x5$start == "1"][(abs(as.numeric(difftime(x5$sak_darbu[x5$start == "1"], x5$beidz_darbu[x5$end == "2"], units = "days"))) == min(abs(as.numeric(difftime(x5$sak_darbu[x5$start == "1"], x5$beidz_darbu[x5$end == "2"], units = "days")))))]), FALSE, x5$sak_darbu == x5$sak_darbu[x5$start == "1"][(abs(as.numeric(difftime(x5$sak_darbu[x5$start == "1"], x5$beidz_darbu[x5$end == "2"], units = "days"))) == min(abs(as.numeric(difftime(x5$sak_darbu[x5$start == "1"], x5$beidz_darbu[x5$end == "2"], units = "days")))))])
        x5_uzDivniekiem <- rbind(x5_uzDivniekiem, x5[logVec_forStart, ], x5[x5$end == "2", ]) 
        rm(logVec_forStart)
      } else if (x5$start[1] == "1" && x5$start[2] == "1" && x5$end[3] == "2" && x5$start[4] == "1" && x5$start[5] == "1" && x5$NDZ_sanemsanas_datums[1] != x5$NDZ_sanemsanas_datums[2]) {
        x5_uzDivniekiem <- rbind(x5_uzDivniekiem, x5[2:3, ])
        x5_uzVieniniekiem <- rbind(x5_uzVieniniekiem, x5[5, ])
      } else {
        stop("ERROR: Šis gadījums funkcijā processingFives nav izstrādāts. Rinda: ", r, ".\n")
      }
    } else if (((x5$end[1] == "2" && x5$start[2] == "1" && x5$NDZ_sanemsanas_datums[1] == x5$NDZ_sanemsanas_datums[2]) || (x5$start[1] == "1" && x5$end[2] == "2" && x5$NDZ_sanemsanas_datums[1] <= x5$NDZ_sanemsanas_datums[2])) && ((x5$start[3] == "1" && x5$end[4] == "2" && x5$NDZ_sanemsanas_datums[3] <= x5$NDZ_sanemsanas_datums[4] && x5$NDZ_sanemsanas_datums[2] != x5$NDZ_sanemsanas_datums[3])||(x5$end[3] == "2" && x5$start[4] == "1" && x5$NDZ_sanemsanas_datums[3] == x5$NDZ_sanemsanas_datums[4] && x5$NDZ_sanemsanas_datums[2] != x5$NDZ_sanemsanas_datums[3])) && (x5$start[5] == "1" && x5$NDZ_sanemsanas_datums[4] != x5$NDZ_sanemsanas_datums[5])){
      x5_uzCetriniekiem <- rbind(x5_uzCetriniekiem, x5[1:4, ])
      x5_uzVieniniekiem <- rbind(x5_uzVieniniekiem, x5[5, ])
    } else if ((x5$start[1] == "1" && x5$start[2] == "1" && x5$end[3] == "2" && x5$start[4] == "1" && x5$end[5] == "2") && (x5$NDZ_sanemsanas_datums[3] != x5$NDZ_sanemsanas_datums[4])) {
      x5_uzCetriniekiem <- rbind(x5_uzCetriniekiem, x5[-1, ])
    } else if ((x5$end[1] == "2" && x5$start[2] == "1" && x5$NDZ_sanemsanas_datums[1] == x5$NDZ_sanemsanas_datums[2]) && (x5$end[3] == "2" && x5$NDZ_sanemsanas_datums[2] != x5$NDZ_sanemsanas_datums[3]) && (x5$start[4] == "1" && x5$NDZ_sanemsanas_datums[3] != x5$NDZ_sanemsanas_datums[4]) && (x5$start[5] == "1" && x5$NDZ_sanemsanas_datums[4] != x5$NDZ_sanemsanas_datums[5]) && ("50" %in% x5$zinkod)) {
      x5_uzVieniniekiem <- rbind(x5_uzVieniniekiem, x5[c(1, 5), ])
      x5_uzDivniekiem <- rbind(x5_uzDivniekiem, x5[2:3, ])
    } else if ((x5$end[1] == "2" && (x5$start[2] == "1"|| x5$end[2] == "2") && x5$NDZ_sanemsanas_datums[1] != x5$NDZ_sanemsanas_datums[2]) && 
               ((x5$start[2] == "1"|| x5$end[2] == "2") && (x5$end[3] == "2" || x5$start[3] == "1") && x5$end[4] == "2" && x5$NDZ_sanemsanas_datums[2] == x5$NDZ_sanemsanas_datums[3] && x5$NDZ_sanemsanas_datums[3] != x5$NDZ_sanemsanas_datums[4]) && 
               (x5$start[5] == "1" && x5$NDZ_sanemsanas_datums[4] != x5$NDZ_sanemsanas_datums[5])) {
      x5_uzVieniniekiem <- rbind(x5_uzVieniniekiem, x5[c(1, 5), ])
      ifelse(x5$start[2] == "1", s <- 2, s <- 3)
      x5_uzDivniekiem <- rbind(x5_uzDivniekiem, x5[c(s, 4), ])
      rm(s)
    } else if ((x5$end[1] == "2" && x5$start[2] == "1" && x5$end[3] == "2" && x5$end[4] == "2" && x5$start[5] == "1" && any(diff(x5$NDZ_sanemsanas_datums[1:4]) != 0) && x5$NDZ_sanemsanas_datums[4] == x5$NDZ_sanemsanas_datums[5]) || (x5$end[1] == "2" && x5$start[2] == "1" && x5$end[3] == "2" && x5$NDZ_sanemsanas_datums[1] != x5$NDZ_sanemsanas_datums[2] && x5$NDZ_sanemsanas_datums[2] != x5$NDZ_sanemsanas_datums[3]) && (x5$start[4] == "1" && x5$end[5] == "2" && x5$NDZ_sanemsanas_datums[3] != x5$NDZ_sanemsanas_datums[4] && x5$NDZ_sanemsanas_datums[4] != x5$NDZ_sanemsanas_datums[5]) || (x5$end[1] == "2" && x5$end[2] == "2" && x5$NDZ_sanemsanas_datums[1] != x5$NDZ_sanemsanas_datums[2] && x5$start[3] == x5$start[4] && x5$NDZ_sanemsanas_datums[2] == x5$NDZ_sanemsanas_datums[3] && x5$end[5] == "2" && x5$NDZ_sanemsanas_datums[4] != x5$NDZ_sanemsanas_datums[5]) || (x5$end[1] == "2" && x5$end[2] == "2" && x5$start[3] == "1" && x5$end[4] == "2" && x5$start[5] == "1" && x5$NDZ_sanemsanas_datums[1] != x5$NDZ_sanemsanas_datums[2] && x5$NDZ_sanemsanas_datums[2] == x5$NDZ_sanemsanas_datums[3] && x5$NDZ_sanemsanas_datums[4] == x5$NDZ_sanemsanas_datums[5])) {
      x5_uzVieniniekiem <- rbind(x5_uzVieniniekiem, x5[1, ]) 
      x5_uzCetriniekiem <- rbind(x5_uzCetriniekiem, x5[-1, ])
    } else if (sum(x5$start == "1") == 3) {
      if((x5$end[1] == "2" && x5$start[2] == "1" && x5$end[3] == "2" && x5$start[4] == "1" && x5$start[5] == "1") && (x5$NDZ_sanemsanas_datums[1] != x5$NDZ_sanemsanas_datums[2] && x5$NDZ_sanemsanas_datums[2] != x5$NDZ_sanemsanas_datums[3] && x5$NDZ_sanemsanas_datums[3] != x5$NDZ_sanemsanas_datums[4] && x5$NDZ_sanemsanas_datums[4] != x5$NDZ_sanemsanas_datums[5])) {
        x5_uzVieniniekiem <- rbind(x5_uzVieniniekiem, x5[c(1, 5), ])
        x5_uzDivniekiem <- rbind(x5_uzDivniekiem, x5[2:3, ])
      } else if ((x5$end[1] == "2" && x5$start[2] == "1" && x5$start[3] == "1" && x5$end[4] == "2" && x5$start[5] == "1") && (x5$NDZ_sanemsanas_datums[1] != x5$NDZ_sanemsanas_datums[2] && x5$NDZ_sanemsanas_datums[2] != x5$NDZ_sanemsanas_datums[3] && x5$NDZ_sanemsanas_datums[3] != x5$NDZ_sanemsanas_datums[4] && x5$NDZ_sanemsanas_datums[4] != x5$NDZ_sanemsanas_datums[5])) {
        x5_uzVieniniekiem <- rbind(x5_uzVieniniekiem, x5[c(1, 5), ])
        x5_uzDivniekiem <- rbind(x5_uzDivniekiem, x5[3:4, ])
      } else if (x5$end[1] == "2" && x5$start[2] == "1" && x5$start[3] == "1" && x5$start[4] == "1" && x5$end[5] == "2" && any(diff(x5$NDZ_sanemsanas_datums) != 0)) {
        x5_uzVieniniekiem <- rbind(x5_uzVieniniekiem, x5[1, ])
        x5_uzDivniekiem <- rbind(x5_uzDivniekiem, x5[4:5, ])
      } else if (x5$start[1] == "1" && x5$end[2] == "2" && x5$start[3] == "1" && x5$end[4] == "2" && x5$start[5] == "1" && any(diff(x5$NDZ_sanemsanas_datums[1:3]) != 0) && x5$NDZ_sanemsanas_datums[4] == x5$NDZ_sanemsanas_datums[5]) {
        x5_uzVieniniekiem <- rbind(x5_uzVieniniekiem, x5[5, ])
        x5_uzDivniekiem <- rbind(x5_uzDivniekiem, x5[1:4, ])
      } else if (x5$end[1] == "2" && x5$start[2] == "1" && x5$start[3] == "1" && x5$end[4] == "2" && x5$start[5] == "1" && x5$NDZ_sanemsanas_datums[1] == x5$NDZ_sanemsanas_datums[2]) {
        x5_uzVieniniekiem <- rbind(x5_uzVieniniekiem, x5[5, ])
        x5_uzDivniekiem <- rbind(x5_uzDivniekiem, x5[1:4, ])
      } else if (all(x5$start[c(1,3,5)] == "1") && 
                 all(diff(x5$NDZ_sanemsanas_datums[2:3]) == 0) && 
                 all(diff(x5$NDZ_sanemsanas_datums[3:5]) != 0) && 
                 all(diff(x5$NDZ_sanemsanas_datums[1:2]) != 0) && 
                 x5$PS_code[1] == "PK195CB6107" && x5$NM_code[1] == "40003005014") {
        # Šis ir tas unokālais gadījums, kluram man vajadzēja visu gadu, lai izlobītu, kas tur notiek, j
        #jo indivīds ik pārdienas ņem bezalgas atvaļinājumus tā, ka atvaļinājuma sākuma dienā atsāk darbu, bet citreiz nē.
        #Nebiju pārliecināta, ka šo var vispārināt.
        x5_uzVieniniekiem <- rbind(x5_uzVieniniekiem, x5[5, ])
        x5_uzCetriniekiem <- rbind(x5_uzCetriniekiem, x5[-5, ])
      } else if (all(x5$start[1:3] == "1") && 
                 all(diff(x5$NDZ_sanemsanas_datums[2:3]) == 0) && 
                 all(diff(x5$NDZ_sanemsanas_datums[1:2]) != 0) && 
                 all(diff(x5$NDZ_sanemsanas_datums[3:5]) != 0)) {
        x5_uzDivniekiem <- rbind(x5_uzDivniekiem, x5[c(3,5), ])
      } else if (all(x5$start[3:5] == "1") && 
           all(diff(x5$NDZ_sanemsanas_datums[1:3]) == 0) && 
           diff(x5$NDZ_sanemsanas_datums[4:5]) == 0 && diff(x5$NDZ_sanemsanas_datums[3:4]) != 0) {
        x5_uzVieniniekiem <- rbind(x5_uzVieniniekiem, x5[c(3,5), ])
      } else if (all(x5$start[3:5] == "1") && 
                 all(diff(x5$NDZ_sanemsanas_datums[1:3]) == 0) && 
                 all(diff(x5$NDZ_sanemsanas_datums[3:5]) != 0)) {
        x5_uzVieniniekiem <- rbind(x5_uzVieniniekiem, x5[c(1,5), ])
      } else {stop("ERROR: Piecinieku gadījumam iztrūkst apstrādes kods: ", paste0("rinda: ", r, "."), "\n")}
    } else if (sum(x5$start == "1") == 2) {
      
      if(x5$end[1] == x5$end[2] && x5$end[2] == "2" && x5$start[3] == x5$start[4] && x5$start[3] == "1" && x5$end[5] == "2" && any(diff(x5$NDZ_sanemsanas_datums) != 0)) {
        x5_uzVieniniekiem <- rbind(x5_uzVieniniekiem, x5[2, ])
        x5_uzDivniekiem <- rbind(x5_uzDivniekiem, x5[4:5, ])
      } else if (x5$start[1] == "1" && x5$end[2] == "2" && x5$end[3] == "2" && x5$start[4] == "1" && x5$end[5] == "2" && x5$NDZ_sanemsanas_datums[3] == x5$NDZ_sanemsanas_datums[4]) {
        x5_uzDivniekiem <- rbind(x5_uzDivniekiem, x5[-3, ])
      } else if (x5$end[1] == "2" && x5$start[2] == "1" && x5$end[3] == "2" && x5$start[4] == "1" && x5$end[5] == "2" && 
                 x5$NDZ_sanemsanas_datums[1] == x5$NDZ_sanemsanas_datums[2] && all(diff(x5$NDZ_sanemsanas_datums[2:5]) != 0) && x5$zinkod[3] == "26") {
        x5_uzVieniniekiem <- rbind(x5_uzVieniniekiem, x5[1, ])
        x5_uzCetriniekiem <- rbind(x5_uzCetriniekiem, x5[-1, ])
      } else if (x5$end[1] == "2" && x5$start[2] == "1" && x5$end[3] == "2" && x5$start[4] == "1" && x5$end[5] == "2" && 
                 x5$NDZ_sanemsanas_datums[3] == x5$NDZ_sanemsanas_datums[4] && all(diff(x5$NDZ_sanemsanas_datums[1:3]) != 0) && 
                 all(diff(x5$NDZ_sanemsanas_datums[4:5]) != 0) && x5$zinkod[3] == "26") {
        x5_uzVieniniekiem <- rbind(x5_uzVieniniekiem, x5[1, ])
        x5_uzCetriniekiem <- rbind(x5_uzCetriniekiem, x5[-1, ])
      } else if (all(x5$start[c(3,5)] == "1") &&  
                 diff(x5$NDZ_sanemsanas_datums[1:2]) == diff(x5$NDZ_sanemsanas_datums[4:5]) &&
                 all(diff(x5$NDZ_sanemsanas_datums[2:4]) != 0) && x5$PS_code[1] == 'PKBAE732743' && x5$NM_code[1] == '40003948702') {
        x5_uzVieniniekiem <- rbind(x5_uzVieniniekiem, x5[c(1,3), ])
      } else if (all(x5$start[c(1,3)] == "1") &&  
                 all(diff(x5$NDZ_sanemsanas_datums) != 0)) {
        x5_uzDivniekiem <- rbind(x5_uzDivniekiem, x5[c(1:3,5), ])
      } else {
        stop(cat("ERROR: Šis gadījums funkcijā processingFives nav izstrādāts. Rinda: ", r, ".\n"))
      }
    }  else {
      stop(cat("ERROR: Piecinieku tabula nepārdalījās. Izejas tabulas x rinda: ", r, ".\n"))
    }
  }
  cat("SKAIDROJUMS: Piecinieku tabula sadalīta tabulās x5_uzVieniniekiem:", nrow(x5_uzVieniniekiem),
      "rindas; x5_uzDivniekiem:", nrow(x5_uzDivniekiem),
      "rindas; un x5_uzCetriniekiem: ", nrow(x5_uzCetriniekiem), "rindas.\n
    Tabulu x5_uzVieniniekiem tālāk pārstrādāt caur processingOnes.\n
    Tabulu x5_uzDivniekiem tālāk pārstrādāt caur processingTwoes.\n
    Tabulu x5_uzCetriniekiem tālāk pārstrādāt caur processingFours.\n ")
  
  rm(x, r, x5)
  
  #3) Apakštabulu x5_uzVieniniekiem apstrādā caur processingOnes function.
  if(nrow(x5_uzVieniniekiem) > 0) {
    x5_uzVieniniekiem <- x5_uzVieniniekiem[order(x5_uzVieniniekiem$PS_code, x5_uzVieniniekiem$NM_code, x5_uzVieniniekiem$NDZ_sanemsanas_datums, x5_uzVieniniekiem$zinkod), ]
    sendTo_tempNDZ(processingOnes(x5_uzVieniniekiem, o))
  } else { 
    cat("Tabula x5_uzVieniniekiem ir tukša.\n")
  }
  
  rm(x5_uzVieniniekiem)
  
  #4) Apakštabulu x5_uzDivniekiem apstrādā caur processingTwoes function.
  if(nrow(x5_uzDivniekiem) > 0) {
    x5_uzDivniekiem <- x5_uzDivniekiem[order(x5_uzDivniekiem$PS_code, x5_uzDivniekiem$NM_code, x5_uzDivniekiem$NDZ_sanemsanas_datums, x5_uzDivniekiem$zinkod), ]
    processingTwoes(x5_uzDivniekiem, o)
  } else { 
    cat("Tabula x5_uzDivniekiem ir tukša.\n")
  }
  
  rm(x5_uzDivniekiem)
  
  #5) Apakštabulu x5_uzCetriniekiem sūta caur processingFours function.
  if(nrow(x5_uzCetriniekiem > 0)) {
    x5_uzCetriniekiem <- x5_uzCetriniekiem[order(x5_uzCetriniekiem$PS_code, x5_uzCetriniekiem$NM_code, x5_uzCetriniekiem$NDZ_sanemsanas_datums, x5_uzCetriniekiem$zinkod), ]
    processingFours(x5_uzCetriniekiem, o)
  } else {
    cat("Tabula x5_uzCetriniekiem ir tukša.\n")
  }

  rm(x5_uzCetriniekiem)
}

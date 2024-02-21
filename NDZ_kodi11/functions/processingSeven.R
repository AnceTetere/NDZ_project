processingSeven <- function(x, o) {
  x <- x[order(x$ps_code, x$dn_code, x$nm_code, x$NDZ_sanemsanas_datums, x$start, x$end), ]
  
  x7_uzVieniniekiem <- data.frame()
  x7_uzDivniekiem <- data.frame()
  x7_uzTrijniekiem <- data.frame()
  x7_uzCetriniekiem <- data.frame()
  x7_uzPieciniekiem <- data.frame()
  x7_uzSesiniekiem <- data.frame()
  check_rows <- 0
  
  for (r in seq(1, nrow(x), by = 7)) {
    x7 <- x[r:(r+6), ]
    x7 <- x7[order(x7$ps_code, x7$dn_code, x7$nm_code, x7$NDZ_sanemsanas_datums, x7$start, x7$end), ]
    
    if(sum(x7$start == "1") == 4) {
      if ((((x7$start[1] == "1" && x7$end[2] == "2")  || (x7$end[1] == "2" && x7$start[2] == "1" && x7$NDZ_sanemsanas_datums[1] == x7$NDZ_sanemsanas_datums[2])) && ((x7$start[3] == "1" && x7$end[4] == "2")  || (x7$end[3] == "2" && x7$start[4] == "1" && x7$NDZ_sanemsanas_datums[3] == x7$NDZ_sanemsanas_datums[4] && x7$NDZ_sanemsanas_datums[2] != x7$NDZ_sanemsanas_datums[3])) && ((x7$start[5] == "1" && x7$end[6] == "2")  || (x7$end[5] == "2" && x7$start[6] == "1" && x7$NDZ_sanemsanas_datums[5] == x7$NDZ_sanemsanas_datums[6]))) && (x7$start[7] == "1")){
        x7_uzVieniniekiem <- rbind(x7_uzVieniniekiem, x7[7, ])
        x7_uzSesiniekiem <- rbind(x7_uzSesiniekiem, x7[-7, ])
        check_rows <- check_rows +7
      } else if (((x7$start[1] == "1" && x7$start[2] == "1" && x7$end[3] == "2") && (x7$NDZ_sanemsanas_datums[1] != x7$NDZ_sanemsanas_datums[2])) && ((x7$start[4] == "1" && x7$end[5] == "2" && x7$end[6] == "2" && x7$start[7] == "1") && (x7$NDZ_sanemsanas_datums[6] != x7$NDZ_sanemsanas_datums[7]))) {
        x7_uzTrijniekiem <- rbind(x7_uzTrijniekiem, x7[1:3, ])
        x7_uzCetriniekiem <- rbind(x7_uzCetriniekiem, x7[4:7, ])
        check_rows <- check_rows +7
      } else if ((x7$end[1] == "2" && x7$start[2] == "1" && x7$end[3] == "2") && (x7$NDZ_sanemsanas_datums[1] != x7$NDZ_sanemsanas_datums[2])) {
        x7_uzTrijniekiem <- rbind(x7_uzTrijniekiem, x7[1:3, ])
        x7_uzCetriniekiem <- rbind(x7_uzCetriniekiem, x7[4:7, ])
        check_rows <- check_rows +7
      } else if (x7$start[1] == "1" && x7$end[2] == "2" && x7$start[3] == "1" && x7$end[4] == "2" && x7$start[5] == "1" && x7$start[6] == "1" && x7$end[7] == "2" && x7$NDZ_sanemsanas_datums[1] != x7$NDZ_sanemsanas_datums[2] && x7$NDZ_sanemsanas_datums[2] != x7$NDZ_sanemsanas_datums[3] && x7$NDZ_sanemsanas_datums[3] != x7$NDZ_sanemsanas_datums[4] && x7$NDZ_sanemsanas_datums[4] != x7$NDZ_sanemsanas_datums[5] && x7$NDZ_sanemsanas_datums[5] != x7$NDZ_sanemsanas_datums[6] && x7$NDZ_sanemsanas_datums[6] != x7$NDZ_sanemsanas_datums[7]) {
        x7_uzCetriniekiem <- rbind(x7_uzCetriniekiem, x7[1:4, ])
        x7_uzDivniekiem <- rbind(x7_uzDivniekiem, x7[6:7, ])
        check_rows <- check_rows +7
      } else {
        stop(cat("Septiņnieku apstrādē, gadījumam rindās", r, "līdz", r + 6, "trūkst izstrādes koda."))
      }
    } else if(sum(x7$end == "2") == 4) {
      if (((x7$end[1] == "2" && x7$start[2] == "1") && (x7$NDZ_sanemsanas_datums[1] != x7$NDZ_sanemsanas_datums[2])) || 
          ((x7$end[1] == x7$end[2]) && (x7$NDZ_sanemsanas_datums[2] == x7$NDZ_sanemsanas_datums[3]))) {
        x7_uzVieniniekiem <- rbind(x7_uzVieniniekiem, x7[1, ])
        x7_uzSesiniekiem <- rbind(x7_uzSesiniekiem, x7[-1, ])
        check_rows <- check_rows +7
      } else {
        stop(cat("Septiņnieku apstrādē, gadījumam rindās", r, "līdz", r + 6, "trūkst izstrādes koda."))
      }
    } else if(sum(x7$start == "1") == 5) {
      if ((x7$start[1] == "1" && x7$end[2] == "2") || ((x7$start[2] == "1" && x7$end[1] == "2") && ((abs(as.numeric(difftime(x7$NDZ_sanemsanas_datums[2], x7$NDZ_sanemsanas_datums[1], units = "days")))) == 0))) {
        x7_uzDivniekiem <- rbind(x7_uzDivniekiem, x7[c(1, 2), ])
        x7_uzPieciniekiem <- rbind(x7_uzPieciniekiem, x7[c(3:7), ])
        check_rows <- check_rows +7
      } else {
        stop(cat("Septiņnieku apstrādē, gadījumam rindās", r, "līdz", r + 6, "trūkst izstrādes koda."))
      }
    } else {
      stop(cat("Septiņnieku apstrādē, gadījumam rindās", r, "līdz", r + 6, "trūkst izstrādes koda."))
    }
  }
  
  #2 PĀRBAUDE: Vai rindu skaits no septiņnieka atvasinātajās tabulās sakrīt ar rindām izejas tabulā x.
  if (check_rows == nrow(x)) {
    cat("PĀRBAUDE IZIETA: Apakštabulu rindu summa sakrīt ar izejošo septiņnieku tabulu.\n")
    rm(x, x7, check_rows)
  } else {
    stop(cat("ERROR: Pārbaude nav izieta: Apakštabulu rindu summa NESAKRĪT ar izejošo septiņnieku tabulu.\n"))
  }
  
  #3 Apakštabulu x7_uzVieniniekiem apstrādā caur processingOnes function.
  if (nrow(x7_uzVieniniekiem) > 0) {
    if (sum(duplicated(x7_uzVieniniekiem[, c("ps_code", "nm_code")])) == 0) {
      x7_uzVieniniekiem <-
        x7_uzVieniniekiem[order(
          x7_uzVieniniekiem$ps_code,
          x7_uzVieniniekiem$nm_code,
          x7_uzVieniniekiem$NDZ_sanemsanas_datums
        ),]
      sendTo_tempNDZ(processingOnes(x7_uzVieniniekiem, o))
    } else {
      stop(cat(
        "No septiņniekiem atvasinātajā tabulā x7_uzVieniniekiem ir dubultnieki."
      ))
    }
  } else {
    cat("Tabula x7_uzVieniniekiem ir tukša.")
  }
  rm(x7_uzVieniniekiem, r)
  
  #4) Apakštabulu x7_uzDivniekiem sūta caur processingTwoes.
  if(nrow(x7_uzDivniekiem) > 0) {
    x7_uzDivniekiem <- x7_uzDivniekiem[order(x7_uzDivniekiem$ps_code, x7_uzDivniekiem$nm_code, x7_uzDivniekiem$NDZ_sanemsanas_datums), ]
    processingTwoes(x7_uzDivniekiem, o)
    cat("Tabula x7_uzDivniekiem pārsūtīta apstrādei caur dubultniekiem, un, pirms nolikšanas izstrādes tabulā temp_NDZ, dienas tiks
      sasummētas uz oriģinālo indivīdu mēnesī definētu kā: period == ps_code == dn_code == nm_code.")
  } else {
    cat("Tabula x7_uzDivniekiem ir tukša.")
  }
  
  rm(x7_uzDivniekiem)
  
  #5) Apakštabulu x7_uzTrijniekiem sūta caur processingThrees.
  if(nrow(x7_uzTrijniekiem) > 0) {
    x7_uzTrijniekiem <- x7_uzTrijniekiem[order(x7_uzTrijniekiem$ps_code, x7_uzTrijniekiem$nm_code, x7_uzTrijniekiem$NDZ_sanemsanas_datums), ]
    processingThrees(x7_uzTrijniekiem, o)
    cat("Tabula x7_uzTrijniekiem pārsūtīta apstrādei caur processingThrees, un, pirms nolikšanas izstrādes tabulā temp_NDZ, dienas tiks
      sasummētas uz oriģinālo indivīdu mēnesī definētu kā: period == ps_code == dn_code == nm_code.")
  } else {
    cat("Tabula x7_uzTrijniekiem ir tukša.")
  }
  
  rm(x7_uzTrijniekiem)
  
  #6) Apakštabulu x7_uzCetriniekiem sūta caur processingThrees.
  if(nrow(x7_uzCetriniekiem) > 0) {
    x7_uzCetriniekiem <- x7_uzCetriniekiem[order(x7_uzCetriniekiem$ps_code, x7_uzCetriniekiem$nm_code, x7_uzCetriniekiem$NDZ_sanemsanas_datums), ]
    processingFours(x7_uzCetriniekiem, o)
    cat("Tabula x7_uzCetriniekiem pārsūtīta apstrādei caur processingFours, un, pirms nolikšanas izstrādes tabulā temp_NDZ, dienas tiks
      sasummētas uz oriģinālo indivīdu mēnesī definētu kā: period == ps_code == dn_code == nm_code.")
  } else {
    cat("Tabula x7_uzCetriniekiem ir tukša.")
  }
  
  rm(x7_uzCetriniekiem)
  
  
  #7) Apakštabulu x7_uzPieciniekiem sūta caur processingFives.
  if(nrow(x7_uzPieciniekiem) > 0) {
    x7_uzPieciniekiem <- x7_uzPieciniekiem[order(x7_uzPieciniekiem$ps_code, x7_uzPieciniekiem$nm_code, x7_uzPieciniekiem$NDZ_sanemsanas_datums), ]
    
    processingFives(x7_uzPieciniekiem, o)
    cat("No septiņniekiem atvasinātā tabula x7_uzPieciniekiem pārsūtīta apstrādei caur processingFives.")
  } else {
    cat("Tabula x7_uzPieciniekiem ir tukša.")
  }
  
  rm(x7_uzPieciniekiem)
  
  #6) Apakštabulu x7_uzSesiniekiem sūta caur processingFives.
  if(nrow(x7_uzSesiniekiem) > 0) {
    x7_uzSesiniekiem <- x7_uzSesiniekiem[order(x7_uzSesiniekiem$ps_code, x7_uzSesiniekiem$nm_code, x7_uzSesiniekiem$NDZ_sanemsanas_datums), ]
    processingSixes(x7_uzSesiniekiem, "6")
    cat("No septiņniekiem atvasinātā tabula x7_uzSesiniekiem pārsūtīta apstrādei caur processingSixes.")
  } else {
    cat("Tabula x7_uzSesiniekiem ir tukša.")
  }
  
  rm(x7_uzSesiniekiem)
}

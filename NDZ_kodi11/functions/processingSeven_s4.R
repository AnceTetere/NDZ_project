processingSeven_s4 <- function(x7s4) {
  x7s4_uzVieniniekiem <- data.frame()
  x7s4_uzDivniekiem <- data.frame()
  x7s4_uzTrijniekiem <- data.frame()
  x7s4_uzCetriniekiem <- data.frame()
  x7s4_uzPieciniekiem <- data.frame()
  x7s4_uzSesiniekiem <- data.frame()
  
  #if ((all(x7s4$sak_beidz[1:2] == c("1", "2")) || (all(x7s4$sak_beidz[1:2] == c("2", "1")) && diff(x7s4$NDZ_sanemsanas_datums[1:2]) == 0))) && ((x7s4$sak_beidz[3] == "1" && x7s4$sak_beidz[4] == "2")  || (x7s4$sak_beidz[3] == "2" && x7s4$sak_beidz[4] == "1" && x7s4$NDZ_sanemsanas_datums[3] == x7s4$NDZ_sanemsanas_datums[4] && x7s4$NDZ_sanemsanas_datums[2] != x7s4$NDZ_sanemsanas_datums[3])) && ((x7s4$sak_beidz[5] == "1" && x7s4$sak_beidz[6] == "2")  || (x7s4$sak_beidz[5] == "2" && x7s4$sak_beidz[6] == "1" && x7s4$NDZ_sanemsanas_datums[5] == x7s4$NDZ_sanemsanas_datums[6]))) && (x7s4$sak_beidz[7] == "1")){
  #  x7s4_uzVieniniekiem <- rbind(x7s4_uzVieniniekiem, x7s4[7, ])
  #  x7s4_uzSesiniekiem <- rbind(x7s4_uzSesiniekiem, x7s4[-7, ])
  #} else 
  #if (((x7s4$sak_beidz[1] == "1" && x7s4$sak_beidz[2] == "1" && x7s4$sak_beidz[3] == "2") && (x7s4$NDZ_sanemsanas_datums[1] != x7s4$NDZ_sanemsanas_datums[2])) && ((x7s4$sak_beidz[4] == "1" && x7s4$sak_beidz[5] == "2" && x7s4$sak_beidz[6] == "2" && x7s4$sak_beidz[7] == "1") && (x7s4$NDZ_sanemsanas_datums[6] != x7s4$NDZ_sanemsanas_datums[7]))) {
  #  x7s4_uzTrijniekiem <- rbind(x7s4_uzTrijniekiem, x7s4[1:3, ])
  #  x7s4_uzCetriniekiem <- rbind(x7s4_uzCetriniekiem, x7s4[4:7, ])
  #} else 
  #if ((x7s4$sak_beidz[1] == "2" && x7s4$sak_beidz[2] == "1" && x7s4$sak_beidz[3] == "2") && (x7s4$NDZ_sanemsanas_datums[1] != x7s4$NDZ_sanemsanas_datums[2])) {
  #  x7s4_uzTrijniekiem <- rbind(x7s4_uzTrijniekiem, x7s4[1:3, ])
  #  x7s4_uzCetriniekiem <- rbind(x7s4_uzCetriniekiem, x7s4[4:7, ])
  #} else 
  #if (x7s4$sak_beidz[1] == "1" && x7s4$sak_beidz[2] == "2" && x7s4$sak_beidz[3] == "1" && x7s4$sak_beidz[4] == "2" && x7s4$sak_beidz[5] == "1" && x7s4$sak_beidz[6] == "1" && x7s4$sak_beidz[7] == "2" && x7s4$NDZ_sanemsanas_datums[1] != x7s4$NDZ_sanemsanas_datums[2] && x7s4$NDZ_sanemsanas_datums[2] != x7s4$NDZ_sanemsanas_datums[3] && x7s4$NDZ_sanemsanas_datums[3] != x7s4$NDZ_sanemsanas_datums[4] && x7s4$NDZ_sanemsanas_datums[4] != x7s4$NDZ_sanemsanas_datums[5] && x7s4$NDZ_sanemsanas_datums[5] != x7s4$NDZ_sanemsanas_datums[6] && x7s4$NDZ_sanemsanas_datums[6] != x7s4$NDZ_sanemsanas_datums[7]) {
  #  x7s4_uzCetriniekiem <- rbind(x7s4_uzCetriniekiem, x7s4[1:4, ])
  #  x7s4_uzDivniekiem <- rbind(x7s4_uzDivniekiem, x7s4[6:7, ])
  #} else 
  if (all(x7s4$sak_beidz == c("1", "2", "1", "1", "2", "1", "2")) && 
      all(sapply(c(1,3:6), function(i) all(diff(x7s4$NDZ_sanemsanas_datums[i:(i+1)]) != 0)))) {
    x7s4_uzTrijniekiem <- rbind(x7s4_uzTrijniekiem, x7s4[1:3, ])
    x7s4_uzCetriniekiem <- rbind(x7s4_uzCetriniekiem, x7s4[4:7, ])
  } else if (all(x7s4$sak_beidz == c("1", "2", "1", "2", "2", "1", "1")) && 
             all(diff(x7s4$NDZ_sanemsanas_datums) != 0)) {
    x7s4_uzCetriniekiem <- rbind(x7s4_uzCetriniekiem, x7s4[c(1:3,5), ])
    x7s4_uzVieniniekiem <- rbind(x7s4_uzVieniniekiem, x7s4[7, ])
  } else if (all(x7s4$sak_beidz == c("2", "1", "1", "2", "1", "2", "1")) && all(diff(x7s4$NDZ_sanemsanas_datums) != 0)) {
    x7s4_uzCetriniekiem <- rbind(x7s4_uzCetriniekiem, x7s4[3:6, ])
    x7s4_uzVieniniekiem <- rbind(x7s4_uzVieniniekiem, x7s4[c(1,7), ])
  } else {
    stop("processingSeven: Septiņnieku apstrādē, gadījumam rindās ", r, " līdz ", r + 6, " trūkst izstrādes koda.\n")
  }
  
  return(list(x7_uzVieniniekiem = x7s4_uzVieniniekiem,
              x7_uzDivniekiem   = x7s4_uzDivniekiem,
              x7_uzTrijniekiem  = x7s4_uzTrijniekiem,
              x7_uzCetriniekiem = x7s4_uzCetriniekiem, 
              x7_uzPieciniekiem = x7s4_uzPieciniekiem,
              x7_uzSesiniekiem  = x7s4_uzSesiniekiem))
}

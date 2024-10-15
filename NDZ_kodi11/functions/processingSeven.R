processingSeven <- function(x, o) {
  x <- arrange(x, PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
  
  x7_uzVieniniekiem <- data.frame()
  x7_uzDivniekiem <- data.frame()
  x7_uzTrijniekiem <- data.frame()
  x7_uzCetriniekiem <- data.frame()
  x7_uzPieciniekiem <- data.frame()
  x7_uzSesiniekiem <- data.frame()
  check_rows <- 0
  
  fncResult <- function(result) {
    x7_uzVieniniekiem <<- rbind(x7_uzVieniniekiem, result$x7_uzVieniniekiem)
    x7_uzDivniekiem   <<- rbind(x7_uzDivniekiem, result$x7_uzDivniekiem)
    x7_uzTrijniekiem  <<- rbind(x7_uzTrijniekiem, result$x7_uzTrijniekiem)
    x7_uzCetriniekiem <<- rbind(x7_uzCetriniekiem, result$x7_uzCetriniekiem)
    x7_uzPieciniekiem <<- rbind(x7_uzPieciniekiem, result$x7_uzPieciniekiem)
    x7_uzSesiniekiem  <<- rbind(x7_uzSesiniekiem, result$x7_uzSesiniekiem)
  }
  
  for (r in seq(1, nrow(x), by = 7)) {
    x7 <- x[r:(r+6), ]
    x7 <- arrange(x7, PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
    
    if(sum(x7$sak_beidz == "1") == 4) {
      result <- processingSeven_s4(x7)
      if (exists("result")) {
        fncResult(result)
        rm(result)
      }
    } else if(sum(x7$sak_beidz == "2") == 4) {
      if ((all(x7$sak_beidz[1:2] == c("2", "1")) && diff(x7$NDZ_sanemsanas_datums[1:2]) != 0) || 
          (x7$sak_beidz[1] == x7$sak_beidz[2] && diff(x7$NDZ_sanemsanas_datums[2:3]) == 0)) {
        x7_uzVieniniekiem <- rbind(x7_uzVieniniekiem, x7[1, ])
        x7_uzSesiniekiem <- rbind(x7_uzSesiniekiem, x7[-1, ])
      } else if (all(x7$sak_beidz == c("2","1","2","1","2","2","1")) && 
                 diff(x7$NDZ_sanemsanas_datums[1:2]) == 0 && 
                 x7$PS_code[1] == '__________' && x7$NM_code[1] == '___________') {
        x7_uzVieniniekiem <- rbind(x7_uzVieniniekiem, x7[1, ])
        x7_uzSesiniekiem <- rbind(x7_uzSesiniekiem, x7[-1, ])
      } else if(all(x7$sak_beidz == c("2", "2", "1", "2", "1", "2", "1")) && all(diff(x7$NDZ_sanemsanas_datums) != 0)) {
        x7_uzVieniniekiem <- rbind(x7_uzVieniniekiem, x7[2, ])
        x7_uzPieciniekiem <- rbind(x7_uzCetriniekiem, x7[3:7, ])
      } else if(all(x7$sak_beidz == c("1", "2", "2", "2", "1", "1", "2")) && 
                diff(x7$NDZ_sanemsanas_datums[4:5]) == 0 && 
                x7$zinkod[3] == "26") {
        x7_uzDivniekiem <- rbind(x7_uzDivniekiem, x7[-2, ])
      } else if(all(x7$sak_beidz == c("2", "2", "1", "1", "2", "2", "1")) && 
                all(sapply(seq(1, 6, by = 2), function(i) diff(x7$NDZ_sanemsanas_datums[i:(i+1)]) == 0))) {
        x7_uzDivniekiem <- rbind(x7_uzDivniekiem, x7[4:5, ])
        x7_uzVieniniekiem <- rbind(x7_uzVieniniekiem, x7[c(1,7), ])
      } else {
        stop("processingSeven: Septiņnieku apstrādē, gadījumam rindās ", r, " līdz ", r + 6, " trūkst izstrādes koda.\n")
      }
    } else if(sum(x7$sak_beidz == "1") == 5) {
      if ((x7$sak_beidz[1] == "1" && x7$sak_beidz[2] == "2") || ((x7$sak_beidz[2] == "1" && x7$sak_beidz[1] == "2") && ((abs(as.numeric(difftime(x7$NDZ_sanemsanas_datums[2], x7$NDZ_sanemsanas_datums[1], units = "days")))) == 0))) {
        x7_uzDivniekiem <- rbind(x7_uzDivniekiem, x7[c(1, 2), ])
        x7_uzPieciniekiem <- rbind(x7_uzPieciniekiem, x7[c(3:7), ])
      } else if (x7$sak_beidz[1] == "1" && x7$sak_beidz[2] == "1" && x7$sak_beidz[3] == "2" && any(diff(x7$NDZ_sanemsanas_datums[1:3]) != 0)) {
        x7_uzSesiniekiem <- rbind(x7_uzSesiniekiem, x7[-1, ])
      } else {
        stop("processingSeven: Septiņnieku apstrādē, gadījumam rindās ", r, " līdz ", r + 6, " trūkst izstrādes koda.\n")
      }
    } else {
      stop("processingSeven: Septiņnieku apstrādē, gadījumam rindās ", r, " līdz ", r + 6, " trūkst izstrādes koda.\n")
    }
    check_rows <- check_rows + 7
  }

#PĀRBAUDE: Vai rindu skaits no septiņnieku atvasinātajās tabulās sakrīt ar rindām sākuma tabulā x.
if (check_rows == nrow(x)) {
    cat("PĀRBAUDE IZIETA: Apakštabulu rindu summa sakrīt ar sākotnējo septiņnieku tabulu.\n")
    rm(x, x7, check_rows, fncResult)
} else {stop("processingSeven: Pārbaude nav izieta, jo apakštabulu rindu summa NESAKRĪT ar sākotnējo septiņnieku tabulu.\n")}
  
#1 Apakštabulu x7_uzVieniniekiem apstrādā caur funkciju processingOnes().
if (nrow(x7_uzVieniniekiem) > 0) {
      x7_uzVieniniekiem <- arrange(x7_uzVieniniekiem, PS_code, NM_code, NDZ_sanemsanas_datums)
      sendTo_tempNDZ(processingOnes(x7_uzVieniniekiem, o))
} else {cat("Tabula x7_uzVieniniekiem ir tukša.\n")}
rm(x7_uzVieniniekiem, r)
  
#2 Apakštabulu x7_uzDivniekiem sūta caur funkciju processingTwoes().
if(nrow(x7_uzDivniekiem) > 0) {
    x7_uzDivniekiem <- arrange(x7_uzDivniekiem, PS_code, NM_code, NDZ_sanemsanas_datums)
    processingTwoes(x7_uzDivniekiem, o)
} else {cat("Tabula x7_uzDivniekiem ir tukša.\n")}
rm(x7_uzDivniekiem)
  
#3 Apakštabulu x7_uzTrijniekiem sūta caur funkciju processingThrees().
if(nrow(x7_uzTrijniekiem) > 0) {
    x7_uzTrijniekiem <- arrange(x7_uzTrijniekiem, PS_code, NM_code, NDZ_sanemsanas_datums)
    processingThrees(x7_uzTrijniekiem, "3")
} else {cat("Tabula x7_uzTrijniekiem ir tukša.\n")}
rm(x7_uzTrijniekiem)
  
#4 Apakštabulu x7_uzCetriniekiem sūta caur funkciju processingFours().
if(nrow(x7_uzCetriniekiem) > 0) {
    x7_uzCetriniekiem <- arrange(x7_uzCetriniekiem, PS_code, NM_code, NDZ_sanemsanas_datums)
    processingFours(x7_uzCetriniekiem, o)
} else {cat("Tabula x7_uzCetriniekiem ir tukša.\n")}
rm(x7_uzCetriniekiem)
  
#5 Apakštabulu x7_uzPieciniekiem sūta caur funkciju processingFives().
if(nrow(x7_uzPieciniekiem) > 0) {
    x7_uzPieciniekiem <- arrange(x7_uzPieciniekiem, PS_code, NM_code, NDZ_sanemsanas_datums)
    processingFives(x7_uzPieciniekiem, o)
} else {cat("Tabula x7_uzPieciniekiem ir tukša.\n")}
rm(x7_uzPieciniekiem)
  
#6 Apakštabulu x7_uzSesiniekiem sūta caur funkciju processingSixes().
if(nrow(x7_uzSesiniekiem) > 0) {
    x7_uzSesiniekiem <- arrange(x7_uzSesiniekiem, PS_code, NM_code, NDZ_sanemsanas_datums)
    processingSixes(x7_uzSesiniekiem, o)
} else {cat("Tabula x7_uzSesiniekiem ir tukša.\n")}
rm(x7_uzSesiniekiem)
}

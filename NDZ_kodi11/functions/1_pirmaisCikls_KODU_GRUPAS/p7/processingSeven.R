processingSeven <- function(x, o, kods) {
  x <- x %>% arrange(PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
  x7_uzVieniniekiem <- data.frame(); x7_uzDivniekiem <- data.frame(); x7_uzTrijniekiem <- data.frame(); x7_uzCetriniekiem <- data.frame(); x7_uzPieciniekiem <- data.frame(); x7_uzSesiniekiem <- data.frame()
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
    x7 <- x[r:(r+6),] %>% arrange(PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
    
  if (sum(x7$sak_beidz == "1") == 4) {
          fncResult(processingSeven_s4(x7, o, kods))
    } else if (sum(x7$sak_beidz == "2") == 4) {
          fncResult(processingSeven_b4(x7))
    } else if (sum(x7$sak_beidz == "1") == 5) {
           if (all(x7$sak_beidz[1:2] == c("1","2")))  {
                  if (all(diff(x7$NDZ_sanemsanas_datums[1:3]) != 0)) {
                    x7_uzDivniekiem <- rbind(x7_uzDivniekiem, x7[c(1, 2), ])
                    x7_uzPieciniekiem <- rbind(x7_uzPieciniekiem, x7[c(3:7), ])
                  } else {stop("processingSeven: Septiņnieku apstrādē, gadījumam rindās ", r, " līdz ", r + 6, " trūkst izstrādes koda.\n")}
            } else if (all(x7$sak_beidz[1:3] == c("1", "1", "2"))) {
                  if (all(diff(x7$NDZ_sanemsanas_datums[1:3]) != 0)) {
                    x7_uzSesiniekiem <- rbind(x7_uzSesiniekiem, x7[-1, ])
                  } else {stop("processingSeven trūkst izstrādes koda.\n")}
            } else if (all(x7$sak_beidz == c("1", "1", "1", "2", "1", "2", "1"))) {
              if (all(diff(x7$NDZ_sanemsanas_datums) != 0)) {
                x7_uzSesiniekiem <- rbind(x7_uzSesiniekiem, x7[-1, ])
              } else {stop("processingSeven trūkst izstrādes koda.\n")}
            } else if (all(x7$sak_beidz[1:3] == c("2", "1", "1"))) {
                   if (diff(x7$NDZ_sanemsanas_datums[1:2]) == 0) {
                     if (x7$PS_code[1] == '__________' && x7$NM_code[1] == '___________') {
                        x7_uzVieniniekiem <- rbind(x7_uzVieniniekiem, x7[1, ])
                        x7_uzSesiniekiem <- rbind(x7_uzSesiniekiem, x7[-1, ])
                     } else {stop("processingSeven trūkst izstrādes koda.\n")}
                   } else {stop("processingSeven trūkst izstrādes koda.\n")}
            } else {stop("processingSeven: Septiņnieku apstrādē, gadījumam rindās ", r, " līdz ", r + 6, " trūkst izstrādes koda.\n")}
    } else {stop("processingSeven: Septiņnieku apstrādē, gadījumam rindās ", r, " līdz ", r + 6, " trūkst izstrādes koda.\n")}
    
    check_rows <- check_rows + 7
  }

#PĀRBAUDE: Vai rindu skaits no septiņnieku atvasinātajās tabulās sakrīt ar rindām sākuma tabulā x.
if (check_rows == nrow(x)) {
    cat("PĀRBAUDE IZIETA: Apakštabulu rindu summa sakrīt ar sākotnējo septiņnieku tabulu.\n"); rm(x, x7, check_rows, fncResult)
} else {stop("processingSeven: Pārbaude nav izieta, jo apakštabulu rindu summa NESAKRĪT ar sākotnējo septiņnieku tabulu.\n")}
  
#1 Apakštabulu x7_uzVieniniekiem apstrādā caur funkciju processingOnes().
if (nrow(x7_uzVieniniekiem) > 0) {
      x7_uzVieniniekiem %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingOnes(o) %>% sendTo_tempNDZ(o)
} else {cat("Tabula x7_uzVieniniekiem ir tukša.\n")}
rm(x7_uzVieniniekiem, r)
  
#2 Apakštabulu x7_uzDivniekiem sūta caur funkciju processingTwoes().
if(nrow(x7_uzDivniekiem) > 0) {
    x7_uzDivniekiem %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingTwoes(o, kods)
} else {cat("Tabula x7_uzDivniekiem ir tukša.\n")}
rm(x7_uzDivniekiem)
  
#3 Apakštabulu x7_uzTrijniekiem sūta caur funkciju processingThrees().
if(nrow(x7_uzTrijniekiem) > 0) {
    x7_uzTrijniekiem %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingThrees(o, kods)
} else {cat("Tabula x7_uzTrijniekiem ir tukša.\n")}
rm(x7_uzTrijniekiem)

#4 Apakštabulu x7_uzCetriniekiem sūta caur funkciju processingFours().
if(nrow(x7_uzCetriniekiem) > 0) {
    x7_uzCetriniekiem %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingFours(o, kods)
} else {cat("Tabula x7_uzCetriniekiem ir tukša.\n")}
rm(x7_uzCetriniekiem)

#5 Apakštabulu x7_uzPieciniekiem sūta caur funkciju processingFives().
if(nrow(x7_uzPieciniekiem) > 0) {
    x7_uzPieciniekiem %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingFives(o, kods)
} else {cat("Tabula x7_uzPieciniekiem ir tukša.\n")}
rm(x7_uzPieciniekiem)

#6 Apakštabulu x7_uzSesiniekiem sūta caur funkciju processingSixes().
if(nrow(x7_uzSesiniekiem) > 0) {
  x7_uzSesiniekiem %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingSixes(o, kods)
} else {cat("Tabula x7_uzSesiniekiem ir tukša.\n")}
rm(x7_uzSesiniekiem)
}

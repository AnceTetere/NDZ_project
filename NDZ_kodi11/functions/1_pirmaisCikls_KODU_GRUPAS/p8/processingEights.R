processingEights <- function(x, o, kods){
  x <- x %>% arrange(PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
  x8_uzVieniniekiem <- data.frame(); x8_uzDivniekiem <- data.frame(); x8_uzCetriniekiem <- data.frame(); x8_uzSesi <- data.frame(); x8_uzSeptini <- data.frame()
  check_rows <- 0

  result <- function(x) {
    x8_uzVieniniekiem <<- rbind(x8_uzVieniniekiem, x$x8_uzVieniniekiem)
    x8_uzDivniekiem <<- rbind(x8_uzDivniekiem, x$x8_uzDivniekiem)
    x8_uzCetriniekiem <<- rbind(x8_uzCetriniekiem, x$x8_uzCetriniekiem)
    x8_uzSesi <<- rbind(x8_uzSesi, x$x8_uzSesi)
    x8_uzSeptini <<- rbind(x8_uzSeptini, x$x8_uzSeptini)
    rm(x)}
  
  for (r in seq(1, nrow(x), by = 8)) {
    x8 <- x[r:(r+7),] %>% arrange(PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
    
    if (x8$PS_code[1] == '_________' && x8$NM_code[1] == '______') {
              if (all(x8$sak_beidz == c("2", "1", "2", "2", "2", "2", "2", "2")) && 
                 diff(x8$NDZ_sanemsanas_datums[1:2]) == 0 && all(diff(x8$NDZ_sanemsanas_datums[2:8]) != 0)) {
                x8_uzVieniniekiem <- rbind(x8_uzVieniniekiem, x8[1:2,])
              } else {stop("processingEights: Iztrūkst kods rindām ", r, " līdz ", r + 7,".\n")}
    } else if (all(x8$sak_beidz[1:2] == c("2", "1")) && diff(x8$NDZ_sanemsanas_datums[1:2]) != 0) {
              x8_uzVieniniekiem <- rbind(x8_uzVieniniekiem, x8[1, ])
              x8_uzSeptini <- rbind(x8_uzSeptini, x8[-1, ])
    } else if (all(x8$sak_beidz == c("2","1","2","1","2","2","1","2")) && 
               diff(x8$NDZ_sanemsanas_datums[1:2]) == 0 && 
               diff(x8$NDZ_sanemsanas_datums[2:3]) != 0 &&
               x8$PS_code[1] == '______' && x8$NM_code[1] == '______') {
              x8_uzDivniekiem <- rbind(x8_uzDivniekiem, x8[c(2,3,4,6,7,8), ])
    } else if (all(x8$sak_beidz[c(1,2,4)] == c("1", "2", "1")) && 
               all(diff(x8$NDZ_sanemsanas_datums[1:3]) != 0) && 
               all(diff(x8$NDZ_sanemsanas_datums[4:6]) != 0) &&
               diff(x8$NDZ_sanemsanas_datums[3:4]) == 0 && 
               diff(x8$NDZ_sanemsanas_datums[6:7] == 0)) {
              x8_uzCetriniekiem <- rbind(x8_uzCetriniekiem, x8)
    } else if (x8$sak_beidz[1] == x8$sak_beidz[2] && all(diff(x8$NDZ_sanemsanas_datums[1:3]) != 0)) {
      x8_uzSeptini <- rbind(x8_uzSeptini, x8[2:8, ])
    } else if (sum(x8$sak_beidz == "1") == 4) {
              result(processingEights_s4(x8, o, kods))
    } else if (sum(x8$sak_beidz == "1") == 6) {
      if (all(x8$sak_beidz == c("1", "2", "1", "1", "2", "1", "1", "1")) && 
          all(diff(x8$NDZ_sanemsanas_datums[1:7]) != 0) && 
          diff(x8$NDZ_sanemsanas_datums[7:8]) == 0 &&
          x8$PS_code[1] == '______' && x8$NM_code[1] == '______') {
        x8_uzVieniniekiem <- rbind(x8_uzVieniniekiem, x8[1,])
      } else {stop("processingEights: Iztrūkst kods rindām ", r, " līdz ", r + 7,".\n")}
    } else if (sum(x8$sak_beidz == "1") == 5) { 
           result(processingEights_s5(x8, o, kods))
    } else {stop("processingEights: Iztrūkst kods rindām ", r, " līdz ", r + 7,".\n")}
    check_rows <- check_rows + 8
  }
  
# PĀRBAUDE: Vai rindu skaits no astoņniekiem atvasinātajās tabulās sakrīt ar rindām sākotnējajā tabulā x.
if (check_rows == nrow(x)) {
  cat("PĀRBAUDE IZIETA: Apakštabulu rindu summa sakrīt ar izejošo tabulu.\n")
  rm(x, x8, check_rows)
} else {stop("processingEights: Pārbaude nav izieta. \n Apakštabulu rindu summa NESAKRĪT ar izejošo tabulu.\n")}
  
#1 Apakštabulu x8_uzVieniniekiem sūta caur processingOnes().
if(nrow(x8_uzVieniniekiem) > 0) {
  x8_uzVieniniekiem %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingOnes(o) %>% sendTo_tempNDZ(o)
} else {cat("Tabula x8_uzVieniniekiem ir tukša.\n")}
  rm(x8_uzVieniniekiem)
  
#2 Apakštabulu x8_uzDivniekiem sūta caur processingTwoes().
if (nrow(x8_uzDivniekiem) > 0) {
  x8_uzDivniekiem %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingTwoes(o, kods)
} else {cat("Tabula x8_uzDivniekiem ir tukša.\n")}
rm(x8_uzDivniekiem)
  
#3 Apakštabulu x8_uzCetriniekiem sūta caur processingFours().
if (nrow(x8_uzCetriniekiem) > 0) {
  x8_uzCetriniekiem %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingFours(o, kods)
} else {cat("Tabula x8_uzCetriniekiem ir tukša.\n")}
rm(x8_uzCetriniekiem)
  
#4 Apakštabulu x8_uzSesi sūta caur processingSixes().
if (nrow(x8_uzSesi) > 0) {
  x8_uzSesi %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingSixes(o, kods)
} else {cat("Tabula x8_uzSesi ir tukša.\n")}
rm(x8_uzSesi)
  
#5 Apakštabulu x8_uzSeptini sūta caur processingSeven().
if (nrow(x8_uzSeptini) > 0) {
  x8_uzSeptini %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingSeven(o, kods)
} else {cat("Tabula x8_uzSeptini ir tukša.\n")}
rm(x8_uzSeptini)
}


processingEights <- function(x, o){
  x <- arrange(x, PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)

  x8_uzVieniniekiem <- data.frame()
  x8_uzDivniekiem <- data.frame()
  x8_uzCetriniekiem <- data.frame()
  x8_uzSesi <- data.frame()
  x8_uzSeptini <- data.frame()
  check_rows <- 0
  
  for (r in seq(1, nrow(x), by = 8)) {
    x8 <- x[r:(r+7), ]
    x8 <- arrange(x8, PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
    
    if (x8$PS_code[1] == '_________' && x8$NM_code[1] == '____________') {
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
               x8$PS_code[1] == '__________' && x8$NM_code[1] == '_________________') {
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
        result <- processingEights_s4(x8)
    } else if (sum(x8$sak_beidz == "1") == 6) {
      if (all(x8$sak_beidz == c("1", "2", "1", "1", "2", "1", "1", "1")) && 
          all(diff(x8$NDZ_sanemsanas_datums[1:7]) != 0) && 
          diff(x8$NDZ_sanemsanas_datums[7:8]) == 0 &&
          x8$PS_code[1] == '_________' && x8$NM_code[1] == '___________') {
        x8_uzVieniniekiem <- rbind(x8_uzVieniniekiem, x8[1,])
      } else {stop("processingEights: Iztrūkst kods rindām ", r, " līdz ", r + 7,".\n")}
    } else if (sum(x8$sak_beidz == "1") == 5) { 
      if(all(x8$sak_beidz[c(1, 3, 5, 7, 8)] == "1") && 
         all(sapply(c(3, 6), function(i) all(diff(x8$NDZ_sanemsanas_datums[i:(i+1)]) == 0))) &&
         all(sapply(c(1,2,4,5,7), function(i) all(diff(x8$NDZ_sanemsanas_datums[i:(i+1)]) != 0)))) {
        x8_uzVieniniekiem <- rbind(x8_uzVieniniekiem, x8[8,])
        x8_uzSesi <- rbind(x8_uzSesi, x8[1:6,])
      } else {stop("processingEights: Iztrūkst kods rindām ", r, " līdz ", r + 7,".\n")}
    } else {stop("processingEights: Iztrūkst kods rindām ", r, " līdz ", r + 7,".\n")}
    
    if(exists("result")) {
    x8_uzVieniniekiem <- rbind(x8_uzVieniniekiem, result$x8_uzVieniniekiem)
    x8_uzDivniekiem <- rbind(x8_uzDivniekiem, result$x8_uzDivniekiem)
    x8_uzCetriniekiem <- rbind(x8_uzCetriniekiem, result$x8_uzCetriniekiem)
    x8_uzSesi <- rbind(x8_uzSesi, result$x8_uzSesi)
    x8_uzSeptini <- rbind(x8_uzSeptini, result$x8_uzSeptini)
    rm(result) # Šis ir ļoti ļoti svarīgi šo aizvākt, citādi ar katru iterāciju viņš dubulto.
    }
    check_rows <- check_rows + 8
  }
  
# PĀRBAUDE: Vai rindu skaits no astoņniekiem atvasinātajās tabulās sakrīt ar rindām sākotnējajā tabulā x.
if (check_rows == nrow(x)) {
  cat("PĀRBAUDE IZIETA: Apakštabulu rindu summa sakrīt ar izejošo tabulu.\n")
  rm(x, x8, check_rows)
} else {stop("processingEights: Pārbaude nav izieta. \n Apakštabulu rindu summa NESAKRĪT ar izejošo tabulu.\n")}
  
#1 Apakštabulu x8_uzVieniniekiem sūta caur processingOnes().
if(nrow(x8_uzVieniniekiem) > 0) {
  x8_uzVieniniekiem <- arrange(x8_uzVieniniekiem, PS_code, NM_code, NDZ_sanemsanas_datums)
  sendTo_tempNDZ(processingOnes(x8_uzVieniniekiem, o))
  cat("No astoņniekiem atvasinātā tabula x8_uzVieniniekiem pārsūtīta apstrādei caur processingOnes().\n")
} else {cat("Tabula x8_uzVieniniekiem ir tukša.\n")}
  rm(x8_uzVieniniekiem)
  
#2 Apakštabulu x8_uzDivniekiem sūta caur processingTwoes().
if (nrow(x8_uzDivniekiem) > 0) {
  x8_uzDivniekiem <- arrange(x8_uzDivniekiem, PS_code, NM_code, NDZ_sanemsanas_datums)
  cat("No desmitniekiem atvasinātā tabula x8_uzDivniekiem pārsūtīta uz processingTwoes() un caur to uz tabulu tempNDZ, ko būvējam.\n")
  processingTwoes(x8_uzDivniekiem, o)
} else {cat("Tabula x8_uzDivniekiem ir tukša.\n")}
  rm(x8_uzDivniekiem)
  
#3 Apakštabulu x8_uzCetriniekiem sūta caur processingFours().
if (nrow(x8_uzCetriniekiem) > 0) {
  x8_uzCetriniekiem <- arrange(x8_uzCetriniekiem, PS_code, NM_code, NDZ_sanemsanas_datums)
  cat("No astoņniekiem atvasinātā tabula x8_uzCetriniekiem pārsūtīta apstrādei caur processingFours.\n")
  processingFours(x8_uzCetriniekiem, o)
} else {cat("Tabula x8_uzCetriniekiem ir tukša.\n")}
  rm(x8_uzCetriniekiem)
  
#4 Apakštabulu x8_uzSesi sūta caur processingSixes().
if (nrow(x8_uzSesi) > 0) {
  x8_uzSesi <- arrange(x8_uzSesi, PS_code, NM_code, NDZ_sanemsanas_datums)
  cat("No astoņniekiem atvasinātā tabula x8_uzSesi pārsūtīta apstrādei caur processingSixes().\n")
  processingSixes(x8_uzSesi, o)
} else {cat("Tabula x8_uzSesi ir tukša.\n")}
  rm(x8_uzSesi)
  
#5 Apakštabulu x8_uzSeptini sūta caur processingSeven().
if (nrow(x8_uzSeptini) > 0) {
  x8_uzSeptini <- arrange(x8_uzSeptini, PS_code, NM_code, NDZ_sanemsanas_datums)
  cat("No astoņniekiem atvasinātā tabula x8_uzSeptini pārsūtīta apstrādei caur processingSeven.\n")
  processingSeven(x8_uzSeptini, o)
} else {cat("Tabula x8_uzSeptini ir tukša.\n")}
  rm(x8_uzSeptini)
}

processingThirteen <- function(x, o, kods) {
  cat("-------------SĀK 13-nieku APSTRĀDI.")
  x <- x %>% arrange(PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)

  x13_uzVieniniekiem <- data.frame()
  x13_uzTrijniekiem <- data.frame()
  x13_uzCetriniekiem <- data.frame()
  x13_uzAstoniekiem <- data.frame()
  x13_uzDesmitniekiem <- data.frame()
  check_rows <- 0
  
  for (r in seq(1, nrow(x), by = 13)) {

    x13 <- x[r:(r+12),] %>% arrange(PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
    
    if (sum(x13$sak_beidz == "2") == 7) {
      if ((x13$sak_beidz[1] == "2") &&(((x13$sak_beidz[2] == "2" && x13$sak_beidz[3] == "1") && (x13$NDZ_sanemsanas_datums[2] == x13$NDZ_sanemsanas_datums[3])) || (x13$sak_beidz[2] == "1" && x13$sak_beidz[3] == "2"))) {
        x13_uzTrijniekiem <- rbind(x13_uzTrijniekiem, x13[1: 3, ])
        x13_uzDesmitniekiem <- rbind(x13_uzDesmitniekiem, x13[4:13, ])
      } else {
        stop("13-nieku tabulas pārdalei trūkst izstrādes koda. Rindas: ", r, " līdz ", r + 12, "\n")
      }
  } else if (sum(x13$sak_beidz == "1") == 7) {
        if (all(x13$sak_beidz[1:3] == c("1","2","1")) && 
          diff(x13$NDZ_sanemsanas_datums[1:2]) >= 0 && 
          diff(x13$NDZ_sanemsanas_datums[2:3]) != 0) {
           if (all(x13$sak_beidz[11:13] == c("1","2","1")) && 
               diff(x13$NDZ_sanemsanas_datums[12:13]) != 0) {
               x13_uzTrijniekiem <- rbind(x13_uzTrijniekiem, x13[11:13, ])
               x13_uzDesmitniekiem <- rbind(x13_uzDesmitniekiem, x13[1:10, ])
          } else if (x13$sak_beidz[9] == x13$sak_beidz[10] && 
                     x13$sak_beidz[11] == x13$sak_beidz[13] && 
                     diff(x13$NDZ_sanemsanas_datums[10:11]) != 0) {
               x13_uzCetriniekiem <- rbind(x13_uzCetriniekiem, x13[10:13, ])
               x13_uzAstoniekiem <- rbind(x13_uzAstoniekiem, x13[1:8, ])
          } else if (all(x13$sak_beidz[c(1, 3, 5, 7, 9, 11, 13)] == "1") && 
                     all(x13$sak_beidz[c(2, 4, 6, 8, 10, 12)] == "2") && 
                     diff(x13$NDZ_sanemsanas_datums[1:12]) != 0 &&
                     diff(x13$NDZ_sanemsanas_datums[12:13]) == 0) {
               x13_uzVieniniekiem <- rbind(x13_uzVieniniekiem, x13[13, ])
               x13_uzCetriniekiem <- rbind(x13_uzCetriniekiem, x13[1:4, ])
               x13_uzAstoniekiem <- rbind(x13_uzAstoniekiem, x13[5:12, ])
          } else {stop("13-nieku tabulas pārdalei trūkst izstrādes koda. Rindas:", r, " līdz ", r + 12, "\n")}
    } else if (all(x13$sak_beidz[c(1, 4, 6, 7, 9, 10, 12)] == "1") && 
                 all(sapply(c(1, 2, 4, 6:12), function(i) all(diff(x13$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) &&
                 all(sapply(seq(3, 6, by = 2), function(i) all(diff(x13$NDZ_sanemsanas_datums[i:(i+1)]) == 0)))) {
        x13_uzCetriniekiem <- rbind(x13_uzCetriniekiem, x13[10:13, ])
        x13_uzAstoniekiem <- rbind(x13_uzAstoniekiem, x13[1:8, ])
      } else if (x13$sak_beidz[11] == "1" && x13$sak_beidz[12] == "2" && x13$sak_beidz[13] == "1" &&
                        all(diff(x13$NDZ_sanemsanas_datums[11:13]) != 0)) { 
          x13_uzTrijniekiem <- rbind(x13_uzTrijniekiem, x13[11:13, ])
          x13_uzDesmitniekiem <- rbind(x13_uzDesmitniekiem, x13[1:10, ])
      } else if (all(x13$sak_beidz[c(2, 4, 6, 8, 9, 12, 13)] == "1") && 
                 all(x13$sak_beidz[c(1, 3, 5, 7, 10, 11)] == "2") && 
                 all(sapply(seq(1, 8, by = 2), function(i) all(diff(x13$NDZ_sanemsanas_datums[i:i+1]) == 0))) &&
                 all(diff(x13$NDZ_sanemsanas_datums[11:12]) == 0) &&
                 x13$NDZ_sanemsanas_datums[12] != x13$NDZ_sanemsanas_datums[13]) {
        x13_uzVieniniekiem <- rbind(x13_uzVieniniekiem, x13[13, ])
        x13_uzCetriniekiem <- rbind(x13_uzCetriniekiem, x13[1:4, ])
        x13_uzAstoniekiem <- rbind(x13_uzAstoniekiem, x13[5:12, ])
        
      } else {
        stop("13-nieku tabulas pārdalei trūkst izstrādes koda. Rindas:",r, "līdz", r + 12, "\n")
    }
    } else {stop("13-nieku tabulas pārdalei trūkst izstrādes koda. Rindas:",r, "līdz", r + 12, "\n")}
    check_rows <- check_rows + 13
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
    x13_uzVieniniekiem %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums, sak_beidz) %>% processingOnes(o) %>% sendTo_tempNDZ(o)
  } else {cat("Tabula x13_uzVieniniekiem ir tukša.\n")}
  rm(x13_uzVieniniekiem)
  
  #4 Apakštabulu x13_uzTrijniekiem sūta caur processingThrees().
  if (nrow(x13_uzTrijniekiem) > 0) {
    x13_uzTrijniekiem %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingThrees(o, kods)
  } else {cat("Tabula x13_uzTrijniekiem ir tukša.\n")}
  rm(x13_uzTrijniekiem)
  
  #5 Apakštabulu x13_uzCetriniekiem sūta caur processingFours().
  if (nrow(x13_uzCetriniekiem) > 0) {
    x13_uzCetriniekiem %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingFours(o, kods)
  } else {cat("Tabula x13_uzCetriniekiem ir tukša.\n")}
  rm(x13_uzCetriniekiem)
  
  #6 Apakštabulu x13_uzAstoniekiem sūta caur processingFours().
  if (nrow(x13_uzAstoniekiem) > 0) {
    x13_uzAstoniekiem %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingEights(o, kods)
  } else {cat("Tabula x13_uzAstoniekiem ir tukša.\n")}
  rm(x13_uzAstoniekiem)
  
  #7 Apakštabulu x13_uzDesmitniekiem sūta caur processingTens().
  if (nrow(x13_uzDesmitniekiem) > 0) {
    x13_uzDesmitniekiem %>% arrange (PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingTens(o, kods)
  } else {cat("Tabula x13_uzDesmitniekiem ir tukša.\n")}
  rm(x13_uzDesmitniekiem)}

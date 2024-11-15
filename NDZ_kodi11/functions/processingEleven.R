processingEleven <- function(x, o) {
  cat("-------------SĀK 11-nieku APSTRĀDI.")
  x <- arrange(x, PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)

  x11_uzVieniniekiem <- data.frame(); x11_uzDevini <- data.frame(); x11_uzDesmitniekiem <- data.frame()
  check_rows <- 0
  
  for (r in seq(1, nrow(x), by = 11)) {
    x11 <- x[r:(r+10),] %>% arrange(PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
      
    if (sum(x11$sak_beidz == "1") == 6) {
      if (all(x11$sak_beidz[c(1,3,5,6,8,10)] == "1") && 
          all(diff(x11$NDZ_sanemsanas_datums[c(1:3, 5:11)]) != 0) &&
          diff(x11$NDZ_sanemsanas_datums[4:5]) == 0) {
          x11_uzDesmitniekiem <- rbind(x11_uzDesmitniekiem, x11[-5,])
      } else if (all(x11$sak_beidz[c(1,3,5,8,9,11)] == "1")) { 
          if (all(diff(x11$NDZ_sanemsanas_datums[c(1:2, 4:7, 9:11)]) != 0) &&
              all(sapply(c(3,7), function(i) diff(x11$NDZ_sanemsanas_datums[i:(i+1)]) == 0))) {
            x11_uzVieniniekiem <- rbind(x11_uzVieniniekiem, x11[11, ])
            x11_uzDesmitniekiem <- rbind(x11_uzDesmitniekiem, x11[-11,]) 
          } else if (all(sapply(c(1:6,8:10), function(i) diff(x11$NDZ_sanemsanas_datums[i:(i+1)]) != 0)) &&
                     diff(x11$NDZ_sanemsanas_datums[7:8]) == 0) {
            x11_uzVieniniekiem <- rbind(x11_uzVieniniekiem, x11[11, ])
            x11_uzDesmitniekiem <- rbind(x11_uzDesmitniekiem, x11[c(1,2,3,4,5,6,8,7,9,10),])
          } else {stop("processingEleven trūkst apstrādes koda. \n")}
      } else if (all(x11$sak_beidz[c(1,4,6,8,10,11)] == "1") && 
                 all(sapply(c(1,2,4,6,8,10), function(i) diff(x11$NDZ_sanemsanas_datums[i:(i+1)]) != 0)) && 
                 all(sapply(seq(3,9,by=2), function(i) diff(x11$NDZ_sanemsanas_datums[i:(i+1)]) == 0))) {
          x11_uzVieniniekiem <- rbind(x11_uzVieniniekiem, x11[11, ])
          x11_uzDesmitniekiem <- rbind(x11_uzDesmitniekiem, x11[-11,])
      } else if (all(x11$sak_beidz[c(1,4,5,7,9,11)] == "1") &&
                 all(sapply(c(2,4,6,8,10), function(i) diff(x11$NDZ_sanemsanas_datums[i:(i+1)]) != 0)) && 
                 all(sapply(c(1,3,7), function(i) diff(x11$NDZ_sanemsanas_datums[i:(i+1)]) == 0))) {
          x11_uzVieniniekiem <- rbind(x11_uzVieniniekiem, x11[11, ])
          x11_uzDesmitniekiem <- rbind(x11_uzDesmitniekiem, x11[-11,])
      } else if (all(x11$sak_beidz[c(2,4,6,8,10,11)] == "1") && all(diff(x11$NDZ_sanemsanas_datums) != 0)) {
          x11_uzVieniniekiem <- rbind(x11_uzVieniniekiem, x11[11, ])
          x11_uzDevini <- rbind(x11_uzDevini, x11[1:9,])
      } else if (all(x11$sak_beidz[seq(1,11,by=2)] == "1") && all(diff(x11$NDZ_sanemsanas_datums) != 0)) {
          x11_uzVieniniekiem <- rbind(x11_uzVieniniekiem, x11[11, ])
          x11_uzDesmitniekiem <- rbind(x11_uzDesmitniekiem, x11[-11,])
      } else if (all(x11$sak_beidz[seq(1,11,by=2)] == "1") && 
                 all(sapply(c(1:6,8:10), function(i) diff(x11$NDZ_sanemsanas_datums[i:(i+1)]) != 0)) &&
                 diff(x11$NDZ_sanemsanas_datums[7:8]) == 0) {
          x11_uzVieniniekiem <- rbind(x11_uzVieniniekiem, x11[11, ])
          x11_uzDesmitniekiem <- rbind(x11_uzDesmitniekiem, x11[-11,])
        } else {stop("processingEleven: Vienpadsmitnieku tabulas pārdalei trūkst izstrādes koda. Rindas: ", r, " līdz ", r + 10, "\n")}
      } else if (sum(x11$sak_beidz == "2") == 6) {
        if (all(x11$sak_beidz[1:2] == c("2", "1")) && diff(x11$NDZ_sanemsanas_datums[1:2]) != 0) {
          x11_uzVieniniekiem <- rbind(x11_uzVieniniekiem, x11[1,])
          x11_uzDesmitniekiem <- rbind(x11_uzDesmitniekiem, x11[2:11,])
        } else {stop("processingEleven: Vienpadsmitnieku tabulas pārdalei trūkst izstrādes koda. Rindas: ", r, " līdz ", r + 10, "\n")}
      } else {stop("processingEleven: Vienpadsmitnieku tabulas pārdalei trūkst izstrādes koda. Rindas: ", r, " līdz ", r + 10, "\n")}
      check_rows <- check_rows + 11
    }
    
#PĀRBAUDE: Vai rindu skaits no vienpadsmitniekiem atvasinātajās tabulās sakrīt ar rindām izejas tabulā x.
if (check_rows == nrow(x)) {
      cat("PĀRBAUDE IZIETA: Apakštabulu rindu summa sakrīt ar izejošo vienpadsmitnieku tabulu.\n"); rm(x, x11, r, check_rows)
} else {stop("processingEleven: PĀRBAUDE NAV IZIETA. Apakštabulu rindu summa NESAKRĪT ar izejošo vienpadsmitnieku tabulu.")}

#1 Apakštabulu x11_uzVieniniekiem sūta caur funkciju processingOnes().
if (nrow(x11_uzVieniniekiem) > 0) {
      cat("No vienpadsmitniekiem atvasinātā tabula x11_uzVieniniekiem pārsūtīta uz processingOnes un tad uz tempNDZ, ko būvējam.\n")
      x11_uzVieniniekiem %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingOnes(o) %>% sendTo_tempNDZ()
} else {cat("Tabula x11_uzVieniniekiem ir tukša.\n")}
rm(x11_uzVieniniekiem)
    
#2 Apakštabulu x11_uzDeviņi sūta caur processingNines().
if (nrow(x11_uzDevini) > 0) {
      cat("No vienpadsmitniekiem atvasinātā tabula x11_uzDevini pārsūtīta uz processingNines() un tad uz tempNDZ, ko būvējam.\n")
      x11_uzDevini %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingNines(o)
} else {cat("Tabula x11_uzDevini ir tukša.\n")}
rm(x11_uzDevini)
    
#3 Apakštabulu x11_uzDesmitniekiem sūta caur processingTens().
if (nrow(x11_uzDesmitniekiem) > 0) {
      cat("No vienpadsmitniekiem atvasinātā tabula x11_uzDesmitniekiem pārsūtīta uz processingTens un tad uz tempNDZ, ko būvējam.\n")
      x11_uzDesmitniekiem %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingTens(o)
} else {cat("Tabula x11_uzDesmitniekiem ir tukša.\n")}
rm(x11_uzDesmitniekiem)
}

processingSixes <- function(x, o) {
  x <- x %>% arrange(PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)

  x6_uzVieniniekiem <- data.frame(); x6_uzDivniekiem <- data.frame(); x6_uzTris <- data.frame(); x6_uzCetri <- data.frame(); x6_uzPieciniekiem <- data.frame()
  check_rows <- 0

  for (r in seq(1, nrow(x), by = 6)) {
      x6 <- x[r:(r+5),] %>% arrange(PS_code, DN_code, NM_code, NDZ_sanemsanas_datums, sak_beidz)
      
    if(all(x6$sak_beidz[1:2] == c("2", "1")) && diff(x6$NDZ_sanemsanas_datums[1:2]) != 0) {
        x6_uzVieniniekiem <- rbind(x6_uzVieniniekiem, x6[1, ])
        x6_uzPieciniekiem <- rbind(x6_uzPieciniekiem, x6[2:6, ])
    } else if (diff(x6$NDZ_sanemsanas_datums[1:2]) == 0 && 
               diff(x6$NDZ_sanemsanas_datums[3:4]) != 0 && 
               x6$sak_beidz[1] != x6$sak_beidz[2] && x6$sak_beidz[3] == "2") {
      x6_uzDivniekiem <- x6[c(1,3), ] %>% rbind(x6_uzDivniekiem)
      x6_uzTris <- x6[4:6, ] %>% rbind(x6_uzTris)
    } else if (sum(x6$sak_beidz == "1") == 6) {
      x_vieninieki <- codes_match(x6)
      x6_uzVieniniekiem <- rbind(x6_uzVieniniekiem, x_vieninieki)
      rm(x_vieninieki)
    } else if (sum(x6$sak_beidz == "1") == 3) {
               if (all(x6$sak_beidz[c(1,3,5)] == "1")) {
                 x6_uzDivniekiem <- rbind(x6_uzDivniekiem, x6)
               } else if(all(x6$sak_beidz == c("1","1","2","2","1","2")) && 
                          all(diff(x6$NDZ_sanemsanas_datums[1:5]) != 0) && 
                          diff(x6$NDZ_sanemsanas_datums[5:6]) == 0) {
                 x6_uzDivniekiem <- rbind(x6_uzDivniekiem, x6[c(2,4,5,6), ])
               } else if(all(x6$sak_beidz == c("2", "2", "1", "1", "2", "1")) && 
                           diff(x6$NDZ_sanemsanas_datums[2:3]) == 0 && all(diff(x6$NDZ_sanemsanas_datums[3:6]) != 0)) {
                 x6_uzVieniniekiem <- rbind(x6_uzVieniniekiem, x6[c(1, 6), ])
                 x6_uzDivniekiem <- rbind(x6_uzDivniekiem, x6[2:5, ])
               } else if (all(x6$sak_beidz == c("2","2","2","1","1","1")) && 
                         diff(x6$NDZ_sanemsanas_datums[3:4]) != 0 && 
                         all(sapply(seq(1,6, by=3), function(i) all(diff(x6$NDZ_sanemsanas_datums[i:(i+2)]) == 0)))) {
                 x6_uzDivniekiem <- rbind(x6_uzDivniekiem, x6[c(1,4), ])
               } else if (all(x6$sak_beidz == c("2", "1", "2", "1", "2", "1")) && 
                         all(sapply(seq(1,4,by=2), function(i) all(diff(x6$NDZ_sanemsanas_datums[i:(i+1)]) == 0))) &&
                         diff(x6$NDZ_sanemsanas_datums[5:6]) != 0 &&
                         x6$PS_code[1] == "___" && x6$NM_code[1] == "___") {
                 x6_uzVieniniekiem <- rbind(x6_uzVieniniekiem, x6[1, ])
                 x6_uzPieciniekiem <- rbind(x6_uzPieciniekiem, x6[-1, ])
               } else if (all(x6$sak_beidz == c("2", "2", "1", "2", "1", "1")) && 
                          all(sapply(seq(2,4, by=2), function(i) all(diff(x6$NDZ_sanemsanas_datums[i:(i+1)]) == 0))) &&
                          all(sapply(seq(1,5, by=2), function(i) all(diff(x6$NDZ_sanemsanas_datums[i:(i+1)]) != 0)))) {
                 x6_uzPieciniekiem <- rbind(x6_uzPieciniekiem, x6[-1, ])
               } else if (all(x6$sak_beidz == c("1","2","1","2","2","1")) && all(diff(x6$NDZ_sanemsanas_datums) != 0)) {
                 x6_uzCetri <- rbind(x6_uzCetri, x6[c(1,2,3,5), ])
                 x6_uzVieniniekiem <- rbind(x6_uzVieniniekiem, x6[6,])
               } else {stop("processingSixes: Sešinieku tabulā trūkst apstrādes koda sešinieku apakštabulai!\n Rinda ", r, " līdz ", r+5, "\n")}
    } else if (sum(x6$sak_beidz == "1") == 4) {
               if(all(x6$sak_beidz == c("1","1","2","1","2","1")) && 
                  all(diff(x6$NDZ_sanemsanas_datums) != 0)) {
                 x6_uzCetri <- rbind(x6_uzCetri, x6[2:5, ])
                 x6_uzVieniniekiem <- rbind(x6_uzVieniniekiem, x6[6, ])
               } else if(all(x6$sak_beidz == c("1","2","1","2","1","1")) &&
                         all(sapply(c(1,2,4,5), function(i) diff(x6$NDZ_sanemsanas_datums[i:(i+1)]) != 0)) &&
                         diff(x6$NDZ_sanemsanas_datums[3:4]) == 0) {
                 x6_uzCetri <- rbind(x6_uzCetri, x6[1:4, ])
                 x6_uzVieniniekiem <- rbind(x6_uzVieniniekiem, x6[6, ])
               } else if(all(x6$sak_beidz == c("1", "2", "1", "1", "2", "1"))) {
                      if(all(sapply(c(1,4), function(i) diff(x6$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
                         all(sapply(c(2,3,5), function(i) diff(x6$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
                        if(x6$PS_code[1] == '___' && x6$NM_code[1] == '___') {
                          x6_uzCetri <- rbind(x6_uzCetri, x6[c(2,3,5,6), ])
                        } else {stop("processingSixes: trūkst apstrādes koda sešinieku apakštabulai!\n Rinda ", r, " līdz ", r+5, "\n")}
                      } else if(all(diff(x6$NDZ_sanemsanas_datums) != 0)) {
                        if(x6$PS_code[1] == '___' && x6$NM_code[1] == '___') {
                          x6_uzCetri <- rbind(x6_uzCetri, x6[c(1,2,4,5), ])
                          x6_uzVieniniekiem <- rbind(x6_uzVieniniekiem, x6[6, ])
                        } else {stop("processingSixes: trūkst apstrādes koda sešinieku apakštabulai!\n Rinda ", r, " līdz ", r+5, "\n")}
                      } else {stop("processingSixes: trūkst apstrādes koda sešinieku apakštabulai!\n Rinda ", r, " līdz ", r+5, "\n")}
               } else {stop("processingSixes: trūkst apstrādes koda sešinieku apakštabulai!\n Rinda ", r, " līdz ", r+5, "\n")}
    } else {stop("processingSixes: Sešinieku tabulā trūkst apstrādes koda sešinieku apakštabulai!\n Rinda ", r, " līdz ", r+5, "\n")}
    check_rows <- check_rows + 6
  }
  
#Pārbaude
if(check_rows == nrow(x)) {
  cat("PĀRBAUDE IZIETA:
      Rindu summa no sešiniekiem atvasinātajās tabulās sakrīt ar rindu skaitu oriģinālajā tabulā NDZ_6.\n")
} else {stop("PĀRBAUDE NAV IZIETA.
             Rindu summa no sešiniekiem atvasinātajās tabulās NESAKRĪT ar rindu skaitu oriģinālajā tabulā NDZ_6.\n")}
rm(x, r, x6, check_rows)
  
#1 Apakštabulu x6_uzVieniniekiem apstrādā caur funkciju processingOnes().
if(nrow(x6_uzVieniniekiem) > 0) {
  x6_uzVieniniekiem %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingOnes(o) %>% sendTo_tempNDZ()
} else {cat("Tabula x6_uzVieniniekiem ir tukša.\n")}
rm(x6_uzVieniniekiem)
  
#2 Apakštabulu x6_uzDivniekiem sūta caur funkciju processingTwoes().
if(nrow(x6_uzDivniekiem) > 0) {
  x6_uzDivniekiem %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingTwoes(o) 
  cat("Atvasinātā tabula x6_uzDivniekiem pārsūtīta apstrādei uz processingTwoes().\n")
} else {cat("Tabula x6_uzDivniekiem ir tukša.\n")}
rm(x6_uzDivniekiem)
  
#3 Apakštabulu x6_uzTris sūta caur funkciju processingThrees().
if(nrow(x6_uzTris) > 0) {
  x6_uzTris %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingThrees(o)
  cat("Atvasinātā tabula x6_uzTris pārsūtīta uz processingFours().\n")
} else {cat("Tabula x6_uzTris ir tukša.\n")}
rm(x6_uzTris)

#4 Apakštabulu x6_uzCetri sūta caur funkciju processingFours().
if(nrow(x6_uzCetri) > 0) {
  x6_uzCetri %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingFours(o)
  cat("Atvasinātā tabula x6_uzCetri pārsūtīta apstrādei caur processingFours().\n")
} else {cat("Tabula x6_uzCetri ir tukša.\n")}
rm(x6_uzCetri)
  
#5 Apakštabulu x6_uzPieciniekiem sūta caur funkciju processingFives().
if(nrow(x6_uzPieciniekiem) > 0) {
  x6_uzPieciniekiem %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingFives(o)
  cat("Atvasinātā tabula x6_uzPieciniekiem no sešinieku pārsūtīta apstrādei caur processingFives().\n")
} else {cat("Tabula x6_uzPieciniekiem ir tukša.\n")}
rm(x6_uzPieciniekiem)
}

processingSixteen <- function(x, o, kods) {
  x <- x %>% arrange(PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
  x16_uz1 <- data.frame(); x16_uzDivniekiem <- data.frame(); x16_uz14 <- data.frame()

  for (r in seq(1, nrow(x), by = 16)) {
    x16 <- x[r:(r+15), ] %>% arrange(PS_code, DN_code, NM_code, NDZ_sanemsanas_datums, zinkod)
    
    if (sum(x16$sak_beidz == "1") == 8) {
            if (x16$sak_beidz[1] == "1") {
              if (all(x16$sak_beidz[2:16] == c("2", "1", "2", "1", "2", "1", "2", "1", "2", "1", "2", "1", "2", "1", "2"))) {
                  if (all(diff(x16$NDZ_sanemsanas_datums) != 0)) {
                      x16_uzDivniekiem <- rbind(x16_uzDivniekiem, x16)
                  } else if (all(sapply(c(1,3,5,9,11,13,15), function(i) diff(x16$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
                             all(sapply(c(2,4,6,7,8,10,12,14), function(i) diff(x16$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
                              if (x16$period[1] == "_____" && x16$PS_code[1] == "_____" && x16$NM_code[1] == "_____") {
                               x16_uzDivniekiem <- rbind(x16_uzDivniekiem, x16)
                              } else {stop("16-nieku tabulā trūkst izstrādes koda. \n")}
                  } else if (all(sapply(c(1,5,7,9,11,13,15), function(i) diff(x16$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
                             all(sapply(c(2,3,4,6,8,10,12,14), function(i) diff(x16$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
                            if (x16$period[1] == "_____" && x16$PS_code[1] == "_____" && x16$NM_code[1] == "_____") {
                              x16_uzDivniekiem <- rbind(x16_uzDivniekiem, x16)
                            } else {stop("16-nieku tabulā trūkst izstrādes koda. \n")}
                  } else if (all(sapply(c(1,3,7,9,11,13,15), function(i) diff(x16$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
                             all(sapply(c(2,4,5,6,8,10,12,14), function(i) diff(x16$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
                            if (x16$period[1] == "_____" && x16$PS_code[1] == "_____" && x16$NM_code[1] == "_____") {
                              x16_uzDivniekiem <- rbind(x16_uzDivniekiem, x16)
                            } else {stop("16-nieku tabulā trūkst izstrādes koda. \n")}
                  } else if (all(sapply(c(9,13,15), function(i) diff(x16$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
                             all(sapply(c(1,2,3,4,5,6,7,8,10,11,12,14), function(i) diff(x16$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
                    if (x16$period[1] == "_____" && x16$PS_code[1] == "_____" && x16$NM_code[1] == "_____") {
                      x16_uzDivniekiem <- rbind(x16_uzDivniekiem, x16)
                    } else {stop("16-nieku tabulā trūkst izstrādes koda. \n")}
                  } else {stop("16-nieku tabulā trūkst izstrādes koda. Rindas: ", r, ":", r+15, "\n")}  
                } else {stop("16-nieku tabulā trūkst izstrādes koda. \n")}
        } else if (x16$sak_beidz[1] == "2") {
              if (all(x16$sak_beidz[2:16] == c("1", "2", "1", "2", "1", "2", "1", "2", "1", "2", "1", "2", "1", "2", "1"))) {
               if (all(diff(x16$NDZ_sanemsanas_datums) != 0)) {
                  x16_uz1 <- rbind(x16_uz1,x16[c(1,16), ])
                  x16_uzDivniekiem <- rbind(x16_uzDivniekiem, x16[2:15, ])
              } else {stop("16-nieku tabulā trūkst izstrādes koda. Rindas: ", r, ":", r+15, "\n")}          
              } else if (all(x16$sak_beidz[2:3] == c("1", "1"))) {
                if (diff(x16$NDZ_sanemsanas_datums[1:2]) == 0 && diff(x16$NDZ_sanemsanas_datums[2:3]) != 0) {
                  x16_uzDivniekiem <- rbind(x16_uzDivniekiem, x16[c(2,1), ])
                  x16_uz14 <- rbind(x16_uz14, x16[3:16, ])
                } else {stop("16-nieku tabulā trūkst izstrādes koda. Rindas: ", r, ":", r+15, "\n")}
            } else {stop("16-nieku tabulā trūkst izstrādes koda. \n")}  
      } else {stop("16-nieku tabulā trūkst izstrādes koda. \n")}
    } else {stop("16-nieku tabulā trūkst izstrādes koda. Rindas: ", r, ":", r+15, "\n")}
    
    if (x16$sak_beidz[1] == "1" && kods %in% c("40", "50", "53") && o == "16") {ZERO_minus(a %>% slice(1))}
    if (x16$sak_beidz[16] == "2" && kods %in% c("40", "50", "53") && o == "16") {ZERO_plus(a %>% slice(16))}
  }
  
  #PĀRBAUDE: Vai rindu skaits no 16-niekiem atvasinātajās tabulās sakrīt ar rindām izejas tabulā x.
  if (nrow(x16_uz1) + nrow(x16_uzDivniekiem) + nrow(x16_uz14) == nrow(x)) {
    cat("PĀRBAUDE IZIETA: Apakštabulu rindu summa sakrīt ar izejošo 16-nieku tabulu.\n")
    rm(x, x16, r)
  } else {stop("PĀRBAUDE NAV IZIETA: Apakštabulu rindu summa NESAKRĪT ar izejošo 16-nieku tabulu.\n")}
  
  #1 Apakštabulu x16_uz1 sūta caur processingOnes().
  if (nrow(x16_uz1) > 0) {
    x16_uz1 <- x16_uz1 %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% 
      processingOnes(o) %>% sendTo_tempNDZ(o)
  } else {cat("Tabula x16_uz1 ir tukša.\n")}
  rm(x16_uz1)
  
  #2 Apakštabulu x16_uzDivniekiem sūta caur processingTwoes().
  if (nrow(x16_uzDivniekiem) > 0) {
    x16_uzDivniekiem %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingTwoes(o, kods) 
  } else {cat("Tabula x16_uzDivniekiem ir tukša.\n")}
  rm(x16_uzDivniekiem)
  
  #3 Apakštabulu x16_x16_uz14 sūta caur processingFourteen().
  if (nrow(x16_uz14) > 0) {
    x16_uz14 %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingFourteen(o, kods) 
  } else {cat("Tabula x16_uz14 ir tukša.\n")}
  rm(x16_uz14)
}

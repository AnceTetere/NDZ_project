processingFifteen <- function(x, o, kods) {
  x <- x %>% arrange(PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
  x15_uz1 <- data.frame(); x15_uz2 <- data.frame(); x15_uzSesi <- data.frame()
  x15_uz13 <- data.frame(); x15_uz14 <- data.frame()
  check_rows <- 0
  
  for (r in seq(1, nrow(x), by = 15)) {
    x15 <- x[r:(r+14),] %>% arrange(PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
    
    if (all(x15$sak_beidz[1:4] == c("2", "1", "2", "1"))) {
            if (all(sapply(c(1,3), function(i) diff(x15$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) && diff(x15$NDZ_sanemsanas_datums[2:3]) != 0) {
              x15_uz2 <- rbind(x15_uz2, x15[c(2,1), ])
              x15_uz13 <- rbind(x15_uz13, x15[-(1:2), ])
            } else if (all(diff(x15$NDZ_sanemsanas_datums) != 0)) {
              x15_uz1 <- rbind(x15_uz1, x15[1, ]); x15_uz14 <- rbind(x15_uz14, x15[-1, ])
            } else {stop("15-niekos trūkst izstrādes kods.")}
    } else if (all(x15$sak_beidz[1:3] == c("1", "2", "1"))) {
            if (all(diff(x15$NDZ_sanemsanas_datums) != 0)) {
              x15_uz2 <- rbind(x15_uz2, x15[1:2, ])
              x15_uz13 <- rbind(x15_uz13, x15[-(1:2), ])
            } else if (all(sapply(c(5,7), function(i) diff(x15$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
                       all(sapply(c(1:4,6,8:14), function(i) diff(x15$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
              x15_uz2 <- rbind(x15_uz2, x15[1:2, ])
              x15_uz13 <- rbind(x15_uz13, x15[-(1:2), ])
            } else {stop("15-niekos trūkst izstrādes kods.")}
    } else if (all(sapply(seq(1,15,by=2), function(i) x15$sak_beidz[i] == "1")) && 
                diff(x15$NDZ_sanemsanas_datums[14:15]) != 0) {
              x15_uz1 <- rbind(x15_uz1, x15[15, ])    
              x15_uz14 <- rbind(x15_uz14, x15[-15, ])    
    } else if (all(sapply(seq(1,15,by=2), function(i) x15$sak_beidz[i] == "2")) && 
               diff(x15$NDZ_sanemsanas_datums[14:15]) != 0) {
              x15_uz1 <- rbind(x15_uz1, x15[1, ])    
              x15_uz14 <- rbind(x15_uz14, x15[-1, ])    
    } else if (all(x15$zinkod == c("40", "91", "41", "92", "40", "91", "41", "92", "40", "91", "41", "92", "40", "91", "92")) &&
               all(sapply(seq(1,13,by=2), function(i) diff(x15$NDZ_sanemsanas_datums[i:(i+1)]) == 0))) {
                x15_uz1 <- rbind(x15_uz1, x15[c(1,15), ])    
                x15_uzSesi <- rbind(x15_uzSesi, x15[seq(3,13,by=2), ])    
    } else {stop("15-nieku tabulas pārdalei trūkst izstrādes koda. Rindas: ", r, " līdz ", r + 14, "\n")}
    check_rows <- check_rows + 15}
  
#PĀRBAUDE: Vai rindu skaits no 15-niekiem atvasinātajās tabulās sakrīt ar rindām izejas tabulā x.
if (check_rows == nrow(x)) {
    cat("PĀRBAUDE IZIETA: Apakštabulu rindu summa sakrīt ar izejošo 15-nieku tabulu.\n"); rm(x, x15, r)
} else {stop("ERROR: Apakštabulu rindu summa NESAKRĪT ar izejošo 15-nieku tabulu.\n")}

#1 Apakštabulu x15_uz1 sūta caur processingOnes().
if (nrow(x15_uz1) > 0) {
    x15_uz1 %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingOnes(o) %>% sendTo_tempNDZ(o)
} else {cat("Tabula x15_uz1 ir tukša.\n")}
rm(x15_uz1)
  
#2 Apakštabulu x15_uzSesi sūta caur processingSixes().
if (nrow(x15_uzSesi) > 0) {
    cat("No 15-niekiem atvasinātā tabula x15_uzSesi pārsūtīta uz processingSixes() un tad uz tempNDZ, ko būvējam.\n")
    x15_uzSesi %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingSixes(o, kods)
} else {cat("Tabula x15_uzSesi ir tukša.\n")}
rm(x15_uzSesi)
      
#3 Apakštabulu x15_uz13 sūta caur processingThirteen().
if (nrow(x15_uz13) > 0) {
  x15_uz13 %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingThirteen(o, kods)
} else {cat("Tabula x15_uz13 ir tukša.\n")}
rm(x15_uz13)


#4 Apakštabulu x15_uz14 sūta caur processingFourteen().
if (nrow(x15_uz14) > 0) {
    x15_uz14 %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingFourteen(o, kods)
} else {cat("Tabula x15_uz14 ir tukša.\n")}
rm(x15_uz14)
}

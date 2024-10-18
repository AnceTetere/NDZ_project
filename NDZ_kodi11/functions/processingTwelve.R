processingTwelve <- function(x, o) {
  cat("-------------SĀK 12-nieku APSTRĀDI.")
  x <- arrange(x, PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
  x12_uzVieniniekiem <- data.frame(); x12_uzDivniekiem <- data.frame(); x12_uzSeptini <- data.frame(); x12_uzDesmitniekiem <- data.frame(); x12_uzVienpadsmit <- data.frame()
  check_rows <- 0
  
  for (r in seq(1, nrow(x), by = 12)) {
    x12 <- x[r:(r+11),] %>% arrange(PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
    
    if (sum(x12$sak_beidz == "2") == 6) {
      if (all(x12$sak_beidz[1:3] == c("2", "1", "2")) && diff(x12$NDZ_sanemsanas_datums[1:2]) != 0) {
        x12_uzVieniniekiem <- rbind(x12_uzVieniniekiem, x12[1, ])
        x12_uzVienpadsmit <- rbind(x12_uzVienpadsmit, x12[2:12, ])
      } else if(all(x12$sak_beidz[1:3] == c("1", "2", "1")) && diff(x12$NDZ_sanemsanas_datums[2:3]) != 0) {
        x12_uzDivniekiem <- rbind(x12_uzDivniekiem, x12[1:2, ])
        x12_uzDesmitniekiem <- rbind(x12_uzDesmitniekiem, x12[-(1:2), ])
      } else if (all(x12$sak_beidz[1:2] == c("2", "1")) && diff(x12$NDZ_sanemsanas_datums[1:2]) == 0) {
        x12_uzDivniekiem <- rbind(x12_uzDivniekiem, x12[1:2, ])
        x12_uzDesmitniekiem <- rbind(x12_uzDesmitniekiem, x12[-(1:2), ])
      } else if (all(x12$sak_beidz[1:2] == c("1", "2")) && 
                 diff(x12$NDZ_sanemsanas_datums[1:2]) >= 0) {
        x12_uzDivniekiem <- rbind(x12_uzDivniekiem, x12[1:2, ])
        x12_uzDesmitniekiem <- rbind(x12_uzDesmitniekiem, x12[-(1:2), ])
      } else if (all(x12$sak_beidz[c(1,2,4)] == "2") && all(x12$sak_beidz[c(3, 5)] == "1") && 
                 diff(x12$NDZ_sanemsanas_datums[1:2]) != 0 &&
                 all(sapply(seq(2, 5, by = 2), function(i) diff(x12$NDZ_sanemsanas_datums[i:(i+1)]) == 0))) {
        x12_uzVieniniekiem <- rbind(x12_uzVieniniekiem, x12[1, ])
        x12_uzVienpadsmit <- rbind(x12_uzVienpadsmit, x12[-1, ])
      } else if (all(x12$sak_beidz[c(3, 4, 6, 8, 10, 12)] == "1") && 
                 all(sapply(c(1, 5, 6, 9, 10), function(i) diff(x12$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
                 all(sapply(c(2, 3, 4, 7, 8, 11), function(i) diff(x12$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
        x12_uzVieniniekiem <- rbind(x12_uzVieniniekiem, x12[1, ])
        x12_uzSeptini <- rbind(x12_uzSeptini, x12[c(4, 7:12), ])
      } else {
        stop("12-nieku tabulas pārdalei trūkst izstrādes koda. Rindas:",r, " līdz ", r + 11)
      }
    } else if (sum(x12$sak_beidz == "1") == 5) {
      if(all(x12$sak_beidz[c(1, 3, 5, 7, 10)] == "1") && all(sapply(c(1:7,9,10), function(i) diff(x12$NDZ_sanemsanas_datums[i:(i+1)]) != 0)) &&
         all(sapply(c(8,11), function(i) diff(x12$NDZ_sanemsanas_datums[i:(i+1)]) == 0))) {
        x12_uzDesmitniekiem <- rbind(x12_uzDesmitniekiem, x12[-c(8,11), ])
      } else {
        stop("processingTwelve: Tabulas pārdalei trūkst izstrādes koda. Rindas: ",r, " līdz ", r + 11)
      }
      
    } else {
      stop("12-nieku tabulas pārdalei trūkst izstrādes koda. Rindas:",r, " līdz ", r + 11)
    }
    check_rows <- check_rows + 12
  }
  
#PĀRBAUDE: Vai rindu skaits no 12-niekiem atvasinātajās tabulās sakrīt ar rindām izejas tabulā x.
  if (check_rows == nrow(x)) {
    cat("PĀRBAUDE IZIETA: Apakštabulu rindu summa sakrīt ar izejošo 12-nieku tabulu.\n"); rm(x, x12, r, check_rows)
  } else {stop("PĀRBAUDE NAV IZIETA: Apakštabulu rindu summa NESAKRĪT ar izejošo 12-nieku tabulu.")}
  
#1 Apakštabulu x12_uzVieniniekiem sūta caur processingOnes().
if (nrow(x12_uzVieniniekiem) > 0) {
    cat("No 12-niekiem atvasinātā tabula x12_uzVieniniekiem pārsūtīta uz processingOnes() un tad uz tempNDZ, ko būvējam.\n")
    x12_uzVieniniekiem %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingOnes(o) %>% sendTo_tempNDZ()
} else {cat("Tabula x12_uzVieniniekiem ir tukša.\n")}
rm(x12_uzVieniniekiem)
  
#2 Apakštabulu x12_uzDivniekiem sūta caur processingTwoes().
if (nrow(x12_uzDivniekiem) > 0) {
    cat("No 12-niekiem atvasinātā tabula x12_uzDivniekiem pārsūtīta uz processingTwoes() un caur to uz tempNDZ, ko būvējam.\n")
    x12_uzDivniekiem %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingTwoes(o)
} else {cat("Tabula x12_uzDivniekiem ir tukša.\n")}
rm(x12_uzDivniekiem) 
  
#3 Apakštabulu x12_uzSeptini sūta caur processingSeven().
if (nrow(x12_uzSeptini) > 0) {
    cat("No 12-niekiem atvasinātā tabula x12_uzSeptini pārsūtīta uz processingSeven() un caur to uz tempNDZ, ko būvējam.\n")
    x12_uzSeptini %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingSeven(o)
} else {cat("Tabula x12_uzSeptini ir tukša.\n")}
rm(x12_uzSeptini)
  
#4 Apakštabulu x12_uzDesmitniekiem sūta caur processingTens().
if (nrow(x12_uzDesmitniekiem) > 0) {
    cat("No 12-niekiem atvasinātā tabula x12_uzDesmitniekiem pārsūtīta uz processingTens un tad uz tempNDZ, ko būvējam.\n")
    x12_uzDesmitniekiem %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingTens(o)
} else {cat("Tabula x12_uzDesmitniekiem ir tukša.\n")}
rm(x12_uzDesmitniekiem)
  
#5 Apakštabulu x12_uzVienpadsmit sūta caur processingEleven().
if (nrow(x12_uzVienpadsmit) > 0) {
    cat("No 12-niekiem atvasinātā tabula x12_uzVienpadsmit pārsūtīta uz processingEleven() un tad uz tempNDZ, ko būvējam.\n")
    x12_uzVienpadsmit %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingEleven(o)
} else {cat("Tabula x12_uzVienpadsmit ir tukša.\n")}
rm(x12_uzVienpadsmit)
}

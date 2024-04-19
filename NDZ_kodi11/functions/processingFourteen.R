processingFourteen <- function(x, o) {
  x <- x[order(x$period, x$pPS_code, x$DN_code, x$NM_code, x$NDZ_sanemsanas_datums, x$start), ]
  
  x14_uzDivi <- data.frame()
  x14_uzDivpadsmit <- data.frame()
  
  for (r in seq(1, nrow(x), by = 14)) {
    
    x14 <- x[r:(r+13),]
    x14 <- x14[order(x14$period, x14$pPS_code, x14$DN_code, x14$NM_code, x14$NDZ_sanemsanas_datums), ]
    
    if (all(x14$start[c(2, 3, 5, 8, 9, 12, 14)] == "1") && 
        x14$NDZ_sanemsanas_datums[1] == x14$NDZ_sanemsanas_datums[2] &&
        x14$NDZ_sanemsanas_datums[2] != x14$NDZ_sanemsanas_datums[3]) {
      x14_uzDivi <- rbind(x14_uzDivi, x14[1:2, ])    
      x14_uzDivpadsmit <- rbind(x14_uzDivpadsmit, x14[-(1:2), ])    
    } else {
      stop("14-nieku tabulas pārdalei trūkst izstrādes koda. Rindas: ", r, " līdz ", r + 13, "\n")
    }
  }
  
  
  #2 PĀRBAUDE: Vai rindu skaits no 14-niekiem atvasinātajās tabulās sakrīt ar rindām izejas tabulā x.
  if (nrow(x14_uzDivi) + nrow(x14_uzDivpadsmit) == nrow(x)) {
    cat("PĀRBAUDE IZIETA: Apakštabulu rindu summa sakrīt ar izejošo 13-nieku tabulu.\n")
    rm(x, x14, r)
  } else {
    stop("ERROR: Apakštabulu rindu summa NESAKRĪT ar izejošo 13-nieku tabulu.\n")
  }
  
  #3 Apakštabulu x14_uzDivi sūta caur processingTwoes().
  if(nrow(x14_uzDivi) > 0) {
    x14_uzDivi <- x14_uzDivi[order(x14_uzDivi$pPS_code, x14_uzDivi$NM_code, x14_uzDivi$NDZ_sanemsanas_datums, x14_uzDivi$start), ]
    cat("No 14-niekiem atvasinātā tabula x14_uzDivi pārsūtīta uz processingTwoes un caur to uz tempNDZ, ko būvējam.\n")
    processingTwoes(x14_uzDivi, o)
  } else {
    cat("Tabula x14_uzDivi ir tukša.\n")
  }
  rm(x14_uzDivi)
  
  #4 Apakštabulu x14_uzDivpadsmit sūta caur processingTwelwe().
  if (nrow(x14_uzDivpadsmit) > 0) {
    x14_uzDivpadsmit <- x14_uzDivpadsmit[order(x14_uzDivpadsmit$pPS_code, x14_uzDivpadsmit$NM_code, x14_uzDivpadsmit$NDZ_sanemsanas_datums, x14_uzDivpadsmit$start),]
    cat("No 14-niekiem atvasinātā tabula x14_uzDivpadsmit pārsūtīta uz processingTwelwe() un caur to uz tempNDZ, ko būvējam.\n")
    processingTwelwe(x14_uzDivpadsmit, o)
  } else {
    cat("Tabula x14_uzDivpadsmit ir tukša.\n")
  }
  
  rm(x14_uzDivpadsmit)
}


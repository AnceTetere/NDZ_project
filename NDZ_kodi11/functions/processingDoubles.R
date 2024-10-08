processingDoubles <- function(x, o) {
  x <- arrange(x, PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
  rownames(x) <- NULL

#1 Pārbaude vai neiztrūkst datums vai ziņojuma kods
if (sum(is.na(x$NDZ_sanemsanas_datums)) > 0 || sum(is.na(x$zinkod)) > 0) {
  stop("processingDoubles() nepilnīgas ailes. \n")
} else {cat("PĀRBAUDE IZIETA: processingDoubles. \n")}

#2 Pārbaude vai datums darba sākšanas kodam nāk pirms darba beigšanas koda, un 
    testR <- 0
    z <- data.frame()
  
  for (O in seq(1, nrow(x), by = 2)) {
    if (x$sak_beidz[O] == '1' && x$sak_beidz[O+1] == '2') {
        y <- x[O :(O + 1),]
        
        if (doublesTest(1, y)) {
          
          if (all(y$zinkod != "26") && y$NDZ_sanemsanas_datums[y$sak_beidz == '1'] < y$NDZ_sanemsanas_datums[y$sak_beidz == '2']){
            y$dienas[1] <- as.numeric(difftime(y$NDZ_sanemsanas_datums[y$sak_beidz == '2'], y$NDZ_sanemsanas_datums[y$sak_beidz == '1'], units = "days"))
            if (y$zinkod[1] %in% c("11", "14", "16", "21", "22", "23", "24", "25", "29")) {
              y$dienas[1] <- y$dienas[1] + 1}}
          
          y$zinkod[1] <- paste0(substr(kodu_tab_nos, nchar(kodu_tab_nos) - 1, nchar(kodu_tab_nos)), "d")
          y$sak_beidz[1] <- "combined" 
          z <- rbind(z, y[1, ])  
          
          testR <- testR + 2
        } else {stop("ERROR in doublesTest: processingDoubles")}
      } else { stop("Rindā ", O, " sak_beidz kodu nesakritība")}}
    
    if (testR == nrow(x)) {cat("PĀRBAUDE IZIETA: processingDoubles. \n"); rm(y, testR, O)
    } else {stop("ERROR: processingDoubles")}
    
    z <- arrange(z, PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
    rm(x)  
    
  return(z)
}

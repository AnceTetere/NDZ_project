processingOnes <- function(x, o) {

    if (as.numeric(o) > 1) { 
      x1 <- x
      x1 <- arrange(x1, PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
	  
      if (nrow(x1) > 0) {      
        x_uzVieniniekiem <- data.frame()
        
        #Te uz vieninieku tabulu atsūtītā tabula tiks pārdalīta apakštabulās x2 un x3.
        #x2 pie sevis ņem tos, kam sak_beidz kodi 2 rindās sakrīt.
        x2 <- data.frame()
        x3 <- data.frame()
        
        if (length(unique(x1$PS_code)) == nrow(x1)) { 
          x_uzVieniniekiem <- x1
        } else {
          r <- 1  # ! šo nedzēs, šis nav no testēšanas palicis r, bet ir nepieciešams while loopam.
          while(r <= nrow(x1)) {
            if (ifelse(is.na(doublesTest(r, x1)), FALSE, doublesTest(r, x1))) {
              if (x1$sak_beidz[r] == x1$sak_beidz[r + 1]) {
                if (x1$zinkod[r] == x1$zinkod[r + 1]) {
                  x2 <- rbind(x2, x1[c(r, r + 1), ])
                } else {
                  x3 <- rbind(x3, x1[c(r, r + 1), ])
                }
              } else if (x1$sak_beidz[r] == "2") {
                x_uzVieniniekiem <- rbind(x_uzVieniniekiem, x1[c(r, r + 1), ])
              }
              r <- r + 2
            } else {
              x_uzVieniniekiem <- rbind(x_uzVieniniekiem, x1[r, ])
              r <- r+1
            }
          }
          rm(r)
        }
        
        
        if (nrow(x1) == sum(nrow(x2), nrow(x3), nrow(x_uzVieniniekiem))) {
          rm(x1)
        } else {
          stop("ERROR: Atvasināto tabulu x2 un x3 nesakrīt ar mātes tabulu x1.")
        }
        
        if (nrow(x2) > 0) {
          x_uzVieniniekiem <- rbind(x_uzVieniniekiem, F_doubleStartEnd_codesMatch(x2))
        }
        
        if (nrow(x3) > 0) {
          x_uzVieniniekiem <- rbind(x_uzVieniniekiem, F_doubleStartEnd_codesDiffer(x3))
        }
        rm(x2, x3)}
      
      x <- x_uzVieniniekiem
      rm(x_uzVieniniekiem)
  }

  #1 Sakārto tabulu
  x <- arrange(x, PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
  rownames(x) <- NULL
  
  x$dienas[x$sak_beidz == "1"] <- as.numeric(difftime(x$last_date[x$sak_beidz == "1"], x$NDZ_sanemsanas_datums[x$sak_beidz == "1"], units = "days")) + 1
  prev <- as.Date(format(x$last_date[1], "%Y-%m-01")) - 1
  
  x$dienas[x$sak_beidz == "2"] <- as.numeric(difftime(x$NDZ_sanemsanas_datums[x$sak_beidz == "2"], prev, units = "days"))
  x$dienas[x$zinkod %in% c("40", "41", "50", "51", "53", "54", "91", "92")] <- x$dienas[x$zinkod %in% c("40", "41", "50", "51", "53", "54", "91", "92")] - 1
  rm(prev)

  x$dienas[x$zinkod == "26"] <- 0
  return(x)
}

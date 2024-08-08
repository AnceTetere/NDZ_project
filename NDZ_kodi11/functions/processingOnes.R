processingOnes <- function(x, o) {
    if (as.numeric(o) > 1) { 
      x1 <- x
      x1 <- x1[order(x1$PS_code, x1$DN_code, x1$NM_code, x1$NDZ_sanemsanas_datums, x1$sak_beidz), ]
      
      if (nrow(x1) > 0) {      
        x_uzVieniniekiem <- data.frame()
        
        x2 <- data.frame()
        x3 <- data.frame()
        
        if (length(unique(x1$PS_code)) == nrow(x1)) { 
          x_uzVieniniekiem <- x1
        } else {
          r <- 1 
          while(r <= nrow(x1)) {
            if (ifelse(is.na(doublesTest(r, x1)), FALSE, doublesTest(r, x1))) {
              if (x1$zinkod[r] == x1$zinkod[r + 1]) {
                x2 <- rbind(x2, x1[c(r, r + 1), ])
              } else {
                x3 <- rbind(x3, x1[c(r, r + 1), ])
              }
              r <- r + 2
            } else {
              x_uzVieniniekiem <- rbind(x_uzVieniniekiem, x1[r, ])
              r <- r+1
            }
          }
        }
        
        
        if (nrow(x1) == sum(nrow(x2), nrow(x3), nrow(x_uzVieniniekiem))) {
          rm(r, x1)
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
  x <- x[order(x$PS_code, x$DN_code, x$NM_code, x$NDZ_sanemsanas_datums, x$sak_beidz), ]
  rownames(x) <- NULL
  
  x$dienas[x$sak_beidz == "1"] <- as.numeric(difftime(x$last_date[x$sak_beidz == "1"], x$NDZ_sanemsanas_datums[x$sak_beidz == "1"], units = "days")) + 1
  
  prev <- as.Date(format(x$last_date[1], "%Y-%m-01")) - 1
  
  x$dienas[x$sak_beidz == "2"] <- as.numeric(difftime(x$NDZ_sanemsanas_datums[x$sak_beidz == "2"], prev, units = "days"))
  
  x$dienas[x$zinkod %in% c("41", "50", "53", "92")] <- x$dienas[x$zinkod %in% c("41", "50", "53", "92")] - 1
  rm(prev)

  x$dienas[x$zinkod == "26"] <- 0
  return(x)
}

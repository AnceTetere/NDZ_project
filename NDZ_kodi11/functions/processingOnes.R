processingOnes <- function(x, o) {
    if (as.numeric(o) > 1) { #šo neņem ārā, šis ir svarīgi, jo tad, kad tas nāk no tabulas NDZ_1, tur visi ir unikālie, un tā tabula ir milzīga, un uzkarina visu programmu dalot to, kad tur nav nekā ko dalīt.

    x1 <- x
    x1 <- x1[order(x1$period, x1$PS_code, x1$DN_code, x1$NM_code, x$NDZ_sanemsanas_datums, x$zinkod),] #saglabāt šo kārtību ir vissvārīgākais šajā projektā
    
    if (nrow(x1) > 0) {
      x_uzVieniniekiem <- data.frame()
      
      #for (p in 1:floor((as.numeric(o) / 2))) {
        x2 <- data.frame()
        x3 <- data.frame()
        
        
        if (length(unique(x1$PS_code)) == nrow(x1)) {
          x_uzVieniniekiem <- x1
        } else {
          r <- 1  # ! šo nedzēs, šis nav no testēšanas palicis r, bet ir nepieciešams while loopam.
          while(r <= nrow(x1)) {
            if (ifelse(is.na(doublesTest(r, x1)), FALSE, doublesTest(r, x1))) {
              if (x1$zinkod[r] == x1$zinkod[r + 1]) {
                x2 <- rbind(x2, x1[c(r, r + 1), ])
                r <- r + 2
              } else if (ifelse(is.na(x1$beidz[r] < x1$sak[r + 1]), FALSE, (x1$beidz[r] < x1$sak[r + 1])))  {
                x_uzVieniniekiem <- rbind(x_uzVieniniekiem, x1[c(r, r + 1), ])
                r <- r + 2
              } else {
                x3 <- rbind(x3, x1[c(r, r + 1), ])
                r <- r + 2
              }
            } else {
              x_uzVieniniekiem <- rbind(x_uzVieniniekiem, x1[r, ])
              r <- r+1
            }
          }
          
          
          if (nrow(x1) == nrow(x2) + nrow(x3) + nrow(x_uzVieniniekiem)) {
            rm(r, x1)
          } else {
            stop(cat(
              "ERROR: Atvasināto tabulu x2 un x3 nesakrīt ar mātes tabulu x1."
            ))
          }
          
          if (nrow(x2) > 0) {
            x_uzVieniniekiem <-
              rbind(x_uzVieniniekiem,
                    F_doubleStartEnd_codesMatch(x2))
          }
          
          if (nrow(x3) > 0) {
            x_uzVieniniekiem <-
              rbind(x_uzVieniniekiem,
                    F_doubleStartEnd_codesDiffer(x3))}
          rm(x2, x3)
        }
      }
      
      x <- x_uzVieniniekiem
      rm(x_uzVieniniekiem)
  }

  #1 Sakārto tabulu
  x <- x[order(x$PS_code, x$DN_code, x$NM_code, x$NDZ_sanemsanas_datums, x$zinkod),]
  rownames(x) <- NULL
  
  #2 Aprēķini dienas   
  x$dienas1 <-
    as.numeric(difftime(x$last_date, x$sak, units = "days")) + 1
  x$dienas[(!is.na(x$sak))] <-
    x$dienas1[(!is.na(x$sak))]
  
  #3 Aprēķini dienas no iepriekšējā mēneša pēdējās dienas līdz beidz, un ievieto tās ailē [dienas]
  prev <- as.Date(format(as.Date(x$last_date[1]), "%Y-%m-01")) - 1
  
  #3.2 sarēķini nostrādātās dienas pret iepriekšējā mēneša pēdējo dienu
  #dienas rēķina no iepriekšējā mēneša beigu datuma līdz datumam [beidz]
  # aizvieto NA vērtības ailē dienas (koda rinda 24).
  x$dienas1 <-
    as.numeric(difftime(as.Date(x$beidz), prev, units = "days"))
  
  if("40" %in% x$zinkod || "50" %in% x$zinkod || "53" %in% x$zinkod || "91" %in% x$zinkod) {
    x$dienas1 <- x$dienas1 - 1
  } 
  
  x$dienas[(!is.na(x$beidz))] <-
    x$dienas1[(!is.na(x$beidz))]
  x$dienas1 <- NULL
  rm(prev)

  x$dienas[x$zinkod == "26"] <- 0
  return(x)

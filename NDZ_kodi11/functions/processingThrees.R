processingThrees <- function(x, o) {
  
  x3_uzDivniekiem <- data.frame()
  x3_uzVieniniekiem <- data.frame()

  for (k in seq(1, nrow(x), by = 3)) {
    #TESTĒŠANAI: k <- sample(seq(1, nrow(x), by = 3), size = 1, replace = F)
  
  x3 <- x[k:(k+2),]
  
    #TESTĒŠANAI: x[x$PS_code == x$PS_code[k], ]
    # Ja visām trim rindām ir sākuma kodi
    if (sum(x3$start == "1") == 3) {
      x3_uzVieniniekiem <- rbind(x3_uzVieniniekiem, codes_match(x3))
      # Ja pirmajām divām rindām ir sākuma kodi tabulā, kurā datumi ailē [NDZ_sanemsanas_datums] kārtoti augošā secībā
    } else if (x3$start[1] == "1" && x3$start[2] == "1") {
        if ((abs(as.numeric(difftime(x3$sak_darbu[x3$start == "1"][1], x3$sak_darbu[x3$start == "1"][2], units = "days"))) <= 14) && (abs(as.numeric(difftime(x3$sak_darbu[x3$start == "1"][1], x3$beidz_darbu[x3$end == "2"], units = "days"))) > 1) && (sum(x3$period == '202201') == 3)) {
        x3_uzDivniekiem <- rbind(x3_uzDivniekiem, cancelOnes(x3))
      } else if ((abs(as.numeric(difftime(x3$sak_darbu[x3$start == "1"][1], x3$sak_darbu[x3$start == "1"][2], units = "days"))) <= 7) && (abs(as.numeric(difftime(x3$sak_darbu[x3$start == "1"][1], x3$beidz_darbu[x3$end == "2"], units = "days"))) > 1)) {
        x3_uzDivniekiem <- rbind(x3_uzDivniekiem, cancelOnes(x3))
      }
    } else {
      result <- splittingThrees(x3)
      
      x3_uzVieniniekiem <- rbind(x3_uzVieniniekiem, result$x_vieninieki)
      x3_uzDivniekiem <- rbind(x3_uzDivniekiem, result$x_divnieki)
      rm(result)
    }
    
    rm(x3)
  }
  
  rm(k, x)
  
  #3) Tabulu x3_uzVieniniekiem sūta caur processingOnes(x, o) funkciju.
  if(nrow(x3_uzVieniniekiem) > 0) {
    sendTo_tempNDZ(processingOnes(x3_uzVieniniekiem, o))
  } else {
    cat("No trijniekiem pārsūtāmajā vieninieku tabulā nebija nevienas rindas.")
  }
  rm(x3_uzVieniniekiem)
  
  
  #4) x3_uzDivniekiem sūta caur processingtwoes function.
  if(nrow(x3_uzDivniekiem) > 0) {
    processingTwoes(x3_uzDivniekiem, o)
  } else {
    cat("No trijniekiem pārsūtāmajā divnieku tabulā nebija nevienas rindas.")
  }
  rm(x3_uzDivniekiem)
}

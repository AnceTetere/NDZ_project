processingThrees <- function(x, o) {

  x <- x[order(x$period, x$PS_code, x$DN_code, x$NM_code, x$NDZ_sanemsanas_datums), ] 
  
  x3_uzDivniekiem <- data.frame()
  x3_uzVieniniekiem <- data.frame()

for (k in seq(1, nrow(x), by = 3)) {

  x3 <- x[k:(k+2),]
  x3 <- x3[order(x3$period, x3$PS_code, x3$DN_code, x3$NM_code, x3$NDZ_sanemsanas_datums), ] 
  
    if (all(x3$sak_beidz[1] == x3$sak_beidz[2], x3$sak_beidz[2] == x3$sak_beidz[3])) {
        x3_uzVieniniekiem <- rbind(x3_uzVieniniekiem, codes_match(x3))
    } else if (all(x3$sak_beidz[1:2] == "1")) {
        if (diff(x3$NDZ_sanemsanas_datums[1:2]) == 0) {
        x3_uzDivniekiem <- rbind(x3_uzDivniekiem, cancelOnes(x3))
      } else if (diff(x3$NDZ_sanemsanas_datums[1:2]) != 0){
        x3_uzDivniekiem <- rbind(x3_uzDivniekiem, cancelOnes(x3))
      }
    } else {
      result <- splittingThrees(x3)
      
      x3_uzVieniniekiem <- rbind(x3_uzVieniniekiem, result$x_vieninieki)
      x3_uzDivniekiem <- rbind(x3_uzDivniekiem, result$x_divnieki)
      rm(result)
    }
  }
  
  rm(k, x, x3)
  
  if(nrow(x3_uzVieniniekiem) > 0) {
    sendTo_tempNDZ(processingOnes(x3_uzVieniniekiem, o))
  } else {
    cat("No trijniekiem pārsūtāmajā vieninieku tabulā nebija nevienas rindas.\n")
  }
  rm(x3_uzVieniniekiem)
  
  
  if(nrow(x3_uzDivniekiem) > 0) {
    processingTwoes(x3_uzDivniekiem, o)
  } else {
    cat("No trijniekiem pārsūtāmajā divnieku tabulā nebija nevienas rindas.\n")
  }
  rm(x3_uzDivniekiem)
}

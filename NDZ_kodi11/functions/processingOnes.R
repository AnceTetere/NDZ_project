processingOnes <- function(x, o) {

  if (as.numeric(o) > 1) { 
    x <- processingOnes_oNotOne(x)
  }

  x <- arrange(x, PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
  rownames(x) <- NULL
  
  x$dienas[x$sak_beidz == "1"] <- as.numeric(difftime(x$last_date[x$sak_beidz == "1"], 
                                                      x$NDZ_sanemsanas_datums[x$sak_beidz == "1"], 
                                                      units = "days")) + 1
  
  prev <- as.Date(format(x$last_date[1], "%Y-%m-01")) - 1
  
  x$dienas[x$sak_beidz == "2"] <- as.numeric(difftime(x$NDZ_sanemsanas_datums[x$sak_beidz == "2"], prev, units = "days"))
  
  x$dienas[x$zinkod %in% c("40", "50", "53", "91")] <- x$dienas[x$zinkod %in% c("40", "50", "53", "91")] - 1
  rm(prev)

  x$dienas[x$zinkod == "26"] <- 0
  return(x)
}

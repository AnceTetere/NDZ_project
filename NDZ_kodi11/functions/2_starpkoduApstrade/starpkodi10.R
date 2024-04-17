starpkodi10 <- function(y2, t, prev, v) {
  
  if (t$zinkod[1] == "51" && 
      t$zinkod[2] == "50" && 
      t$zinkod[3] == "51" && 
      t$zinkod[4] == "50" && 
      t$zinkod[5] == "51" && 
      t$zinkod[6] == "50" && 
      t$zinkod[7] == "51" && 
      t$zinkod[8] == "50" && 
      t$zinkod[9] == "21" && 
      t$zinkod[10] == "51" && 
      all(!diff(t$NDZ_sanemsanas_datums[1:9]) == 0) &&
      t$NDZ_sanemsanas_datums[9] == t$NDZ_sanemsanas_datums[10]) {

    yt <- y2[v:(v+1), ]
    yt <- yt[yt$zinkod == "50", ]
  } else if (t$zinkod[1] == "51" && 
             t$zinkod[2] == "50" && 
             t$zinkod[3] == "51" && 
             t$zinkod[4] == "50" && 
             t$zinkod[5] == "51" && 
             t$zinkod[6] == "50" && 
             t$zinkod[7] == "51" && 
             t$zinkod[8] == "50" && 
             t$zinkod[9] == "51" && 
             t$zinkod[10] == "25" && 
             all(diff(t$NDZ_sanemsanas_datums) != 0)) {
    days1 <- as.numeric(difftime(t$beidz[2], t$sak[1], units = "days"))
    days2 <- as.numeric(difftime(t$beidz[4], t$sak[3], units = "days"))
    days3 <- as.numeric(difftime(t$beidz[6], t$sak[5], units = "days"))
    days4 <- as.numeric(difftime(t$beidz[8], t$sak[7], units = "days"))
    days5 <- as.numeric(difftime(t$beidz[10], t$sak[9], units = "days")) + 1
    
    yt <- y2[v, ]
    yt$dienas <- sum(days1, days2, days3, days4, days5)
    rm(days1, days2, days3, days4, days5)
  } else {
    stop("Starpkodi10 iztrūkst apstrādes koda.")
  }
  
  if(is.na(yt$PS_code[1])) {stop("Dienas NA.")}
  yt$zinkod <- "combined"  #jo starpkodu dienu sarēķins
  return(yt)
}

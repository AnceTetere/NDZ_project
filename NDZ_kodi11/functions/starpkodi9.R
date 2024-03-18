starpkodi9 <- function(y2, t, prev) {
  
  if (t$zinkod[1] == "11" && 
      t$zinkod[2] == "81" && 
      t$zinkod[3] == "25" && 
      t$zinkod[4] == "82" && 
      t$zinkod[5] == "11" && 
      t$zinkod[6] == "81" && 
      t$zinkod[7] == "25" && 
      t$zinkod[8] == "82" && 
      t$zinkod[9] == "11" && 
      t$NDZ_sanemsanas_datums[1] == t$NDZ_sanemsanas_datums[2] && 
      t$NDZ_sanemsanas_datums[3] == t$NDZ_sanemsanas_datums[4] && 
      t$NDZ_sanemsanas_datums[5] == t$NDZ_sanemsanas_datums[6] && 
      t$NDZ_sanemsanas_datums[7] == t$NDZ_sanemsanas_datums[8] &&
      t$NDZ_sanemsanas_datums[8] != t$NDZ_sanemsanas_datums[9]) {

    yt <- y2[v:(v+1), ][yt$zinkod == "11", ]
  } else if (t$zinkod[1] == "50" && 
             t$zinkod[2] == "51" && 
             t$zinkod[3] == "50" && 
             t$zinkod[4] == "51" && 
             t$zinkod[5] == "50" && 
             t$zinkod[6] == "51" && 
             t$zinkod[7] == "50" && 
             t$zinkod[8] == "25" && 
             t$zinkod[9] == "51" && 
             t$NDZ_sanemsanas_datums[8] == t$NDZ_sanemsanas_datums[9] &&
             all(!diff(t$NDZ_sanemsanas_datums[1:8]) == 0)) {

    days1 <- as.numeric(difftime(t$beidz[1], prev, units = "days")) - 1 #jo atvaļinājums
    days2 <- as.numeric(difftime(t$beidz[3], t$sak[2], units = "days"))
    days3 <- as.numeric(difftime(t$beidz[5], t$sak[4], units = "days"))
    days4 <- as.numeric(difftime(t$beidz[7], t$sak[6], units = "days"))
    days5 <- as.numeric(difftime(t$beidz[8], t$sak[9], units = "days")) + 1 #jo darbs
    days <- days1 + days2 + days3 + days4 + days5
    rm(days1, days2, days3, days4, days5)
    
    yt <- y2[v, ]
    yt$dienas <- days
  } else {
    stop("Starpkodi9 iztrūkst apstrādes koda.")
  }
  
  yt$zinkod <- "combined"  #jo starpkodu dienu sarēķins
  return(yt)
  rm(days, yt)
}

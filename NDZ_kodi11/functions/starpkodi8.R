starpkodi8 <- function(y2, t, prev) {
  
  if (t$zinkod[1] == "25" && 
      t$zinkod[2] == "11" && 
      t$zinkod[3] == "11" && 
      t$zinkod[4] == "81" && 
      t$zinkod[5] == "25" && 
      t$zinkod[6] == "82" && 
      t$zinkod[7] == "25" && 
      t$zinkod[8] == "11" && 
      t$NDZ_sanemsanas_datums[1] == t$NDZ_sanemsanas_datums[2] && 
      t$NDZ_sanemsanas_datums[3] == t$NDZ_sanemsanas_datums[4] && 
      t$NDZ_sanemsanas_datums[5] == t$NDZ_sanemsanas_datums[6] && 
      t$NDZ_sanemsanas_datums[7] == t$NDZ_sanemsanas_datums[8]) {

    yt <- y2[v, ]
    yt$dienas <- as.numeric(difftime(as.Date(t$beidz[5]), as.Date(t$saak[3]), units = "days")) + 1 + 2
  } else if (t$zinkod[1] == "11" && 
             t$zinkod[2] == "50" && 
             t$zinkod[3] == "51" && 
             t$zinkod[4] == "50" && 
             t$zinkod[5] == "51" && 
             t$zinkod[6] == "50" && 
             t$zinkod[7] == "51" && 
             t$zinkod[8] == "50" && 
             all(!diff(t$NDZ_sanemsanas_datums) == 0)) {

    days1 <- as.numeric(difftime(as.Date(t$beidz[2]), as.Date(t$saak[1]), units = "days"))
    days2 <- as.numeric(difftime(as.Date(t$beidz[4]), as.Date(t$saak[3]), units = "days"))
    days3 <- as.numeric(difftime(as.Date(t$beidz[6]), as.Date(t$saak[5]), units = "days"))
    days4 <- as.numeric(difftime(as.Date(t$beidz[8]), as.Date(t$saak[7]), units = "days"))
    days = days1 + days2 + days3 + days4
    rm(days1, days2, days3, days4)
    
    yt <- y2[v, ]
    yt$dienas <- days
  } else if (t$zinkod[1] == "50" && 
             t$zinkod[2] == "51" && 
             t$zinkod[3] == "50" && 
             t$zinkod[4] == "51" && 
             t$zinkod[5] == "50" && 
             t$zinkod[6] == "51" && 
             t$zinkod[7] == "50" && 
             t$zinkod[8] == "25" && 
             all(!diff(t$NDZ_sanemsanas_datums) == 0)) {

    days1 <- as.numeric(difftime(t$beidz[1], prev, units = "days")) - 1 #jo atvaļinājums
    days2 <- as.numeric(difftime(as.Date(t$beidz[3]), as.Date(t$saak[2]), units = "days"))
    days3 <- as.numeric(difftime(as.Date(t$beidz[5]), as.Date(t$saak[4]), units = "days"))
    days4 <- as.numeric(difftime(as.Date(t$beidz[7]), as.Date(t$saak[6]), units = "days")) #un tad, kad tiek atlaists, jau ir atvaļinājumā.
    days = days1 + days2 + days3 + days4
    rm(days1, days2, days3, days4)
    
    yt <- y2[v, ]
    yt$dienas <- days
  } else if (t$zinkod[1] == "11" && 
             t$zinkod[2] == "26" && 
             t$zinkod[3] == "81" && 
             t$zinkod[4] == "82" && 
             t$zinkod[5] == "11" && 
             t$zinkod[6] == "81" && 
             t$zinkod[7] == "25" && 
             t$zinkod[8] == "82" && 
             all(diff(t$NDZ_sanemsanas_datums[1:4]) == 0) &&
             t$NDZ_sanemsanas_datums[5] == t$NDZ_sanemsanas_datums[6] &&
             t$NDZ_sanemsanas_datums[7] == t$NDZ_sanemsanas_datums[8]) {
    
    yt <- y2[v:(v+1), ]
    yt <- yt[yt$zinkod == "11", ]
  } else if (t$zinkod[1] == "11" && 
             t$zinkod[2] == "26" && 
             t$zinkod[3] == "82" && 
             t$zinkod[4] == "81" && 
             t$zinkod[5] == "11" && 
             t$zinkod[6] == "81" && 
             t$zinkod[7] == "25" && 
             t$zinkod[8] == "82" && 
             all(diff(t$NDZ_sanemsanas_datums[1:4]) == 0) &&
             t$NDZ_sanemsanas_datums[5] == t$NDZ_sanemsanas_datums[6] &&
             t$NDZ_sanemsanas_datums[7] == t$NDZ_sanemsanas_datums[8]) {
    
    yt <- y2[v:(v+1), ]
    yt <- yt[yt$zinkod == "11", ]
  } else {
    stop("Starpkodi8 iztrūkst apstrādes koda.")
  }
  
  yt$zinkod <- "combined"  #jo starpkodu dienu sarēķins
  return(yt)
  rm(days, yt)
}

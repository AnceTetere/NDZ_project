starpkodi4 <- function(y2, t, prev) {
  
  if (t$zinkod[1] == "11" && t$zinkod[2] == "50" && 
      t$NDZ_sanemsanas_datums[1] == t$NDZ_sanemsanas_datums[2] && 
      t$zinkod[3] == "51" && t$zinkod[4] == "21" && 
      t$NDZ_sanemsanas_datums[3] != t$NDZ_sanemsanas_datums[4]) {
    days <- as.numeric(difftime(as.Date(t$beidz_darbu[4]), as.Date(t$sak_darbu[3]), units = "days")) 
    yt <- y2[v, ]
    yt$dienas <- days
  } else if (t$zinkod[1] == "11" && t$zinkod[2] == "50" && 
             t$NDZ_sanemsanas_datums[1] == t$NDZ_sanemsanas_datums[2] && 
             t$zinkod[3] == "51" && t$zinkod[4] == "25" && 
             t$NDZ_sanemsanas_datums[3] != t$NDZ_sanemsanas_datums[4]) {
    days <- as.numeric(difftime(as.Date(t$beidz_darbu[4]), as.Date(t$sak_darbu[3]), units = "days")) 
    yt <- y2[v, ]
    yt$dienas <- days
  } else if (t$zinkod[1] == "25" && t$zinkod[2] == "51" && 
             t$NDZ_sanemsanas_datums[1] == t$NDZ_sanemsanas_datums[2] && 
             t$zinkod[3] == "11" && t$zinkod[4] == "25" && 
             t$NDZ_sanemsanas_datums[3] != t$NDZ_sanemsanas_datums[4]) {
    days <- as.numeric(difftime(as.Date(t$beidz_darbu[4]), as.Date(t$sak_darbu[3]), units = "days")) 
    yt <- y2[v, ]
    yt$dienas <- days
  } else if (t$zinkod[1] == "11" && t$zinkod[2] == "50" && 
             t$zinkod[3] == "51" && t$zinkod[4] == "25" && 
             all(diff(t$NDZ_sanemsanas_datums) != 0)) {
    days1 <- as.numeric(difftime(as.Date(t$beidz_darbu[2]), as.Date(t$sak_darbu[1]), units = "days")) 
    days2 <- as.numeric(difftime(as.Date(t$beidz_darbu[4]), as.Date(t$sak_darbu[3]), units = "days")) 
    days <- (days1 + days2)
    rm(days1, days2)
    
    yt <- y2[v, ]
    yt$dienas <- days
  } else if (t$zinkod[1] == "51" && t$zinkod[2] == "50" && 
             t$zinkod[3] == "25" && t$zinkod[4] == "51" && 
             t$NDZ_sanemsanas_datums[1] != t$NDZ_sanemsanas_datums[2] &&
             t$NDZ_sanemsanas_datums[3] == t$NDZ_sanemsanas_datums[4]) {
    days <- as.numeric(difftime(as.Date(t$beidz_darbu[2]), as.Date(t$sak_darbu[1]), units = "days")) 

    yt <- y2[v, ]
    yt$dienas <- days
  } else {
    stop("Starpkodi4 iztrūkst apstrādes koda.")
  }
  
  yt$zinkod <- "combined"  #jo starpkodu dienu sarēķins
  return(yt)
  rm(days, yt)
}

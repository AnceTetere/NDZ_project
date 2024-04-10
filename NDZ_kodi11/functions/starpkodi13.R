starpkodi13 <- function(y2, t, prev) {
  if (t$zinkod[1] == "50" && 
      t$zinkod[2] == "51" && 
      t$zinkod[3] == "50" && 
      t$zinkod[4] == "51" && 
      t$zinkod[5] == "50" && 
      t$zinkod[6] == "51" && 
      t$zinkod[7] == "50" && 
      t$zinkod[8] == "51" && 
      t$zinkod[9] == "50" && 
      t$zinkod[10] == "51" && 
      t$zinkod[11] == "50" && 
      t$zinkod[12] == "51" && 
      t$zinkod[13] == "25" && 
      all(!diff(t$NDZ_sanemsanas_datums) == 0)) {
    #Indivīds ņem daudzus bezalgas atvaļinājumus, līdz tiek atlaists.
    days1 <- as.numeric(difftime(t$beidZ[1], prev, units = "days")) - 1 #jo atvaļinājums 
    days2 <- as.numeric(difftime(t$beidZ[3], t$sak[2], units = "days")) 
    days3 <- as.numeric(difftime(t$beidZ[5], t$sak[4], units = "days")) 
    days4 <- as.numeric(difftime(t$beidZ[7], t$sak[6], units = "days")) 
    days5 <- as.numeric(difftime(t$beidZ[9], t$sak[8], units = "days")) 
    days6 <- as.numeric(difftime(t$beidZ[11], t$sak[10], units = "days")) 
    days7 <- as.numeric(difftime(t$beidZ[13], t$sak[12], units = "days")) 
    days <- sum(days1, days2, days3, days4, days5, days6, days7)
    rm(days1, days2, days3, days4, days5, days6, days7)
    
    yt <- y2[v, ]
    yt$dienas <- days
  } else {
    stop("Starpkodi13 iztrūkst apstrādes koda.")
  }

  if(is.na(yt$PS_code[1])) {stop("Dienas NA.")}
  yt$zinkod <- "combined"  #jo starpkodu dienu sarēķins
  return(yt)
}

starpkodi3 <- function(y2, t, prev) {
  if (t$zinkod[1] == "50" && t$zinkod[2] == "21" && t$zinkod[3] == "51" && t$NDZ_sanemsanas_datums[2] == t$NDZ_sanemsanas_datums[3]){
    zk <- "50"
    yt <- y2[v:(v+1), ]
  } else if (t$zinkod[1] == "50" && t$zinkod[2] == "51" && t$zinkod[3] == "25" && !any(diff(t$NDZ_sanemsanas_datums) == 0)) {
    
    days1 <- as.numeric(difftime(as.Date(t$beidz_darbu[1]), prev, units = "days"))
    days2 <- as.numeric(difftime(as.Date(t$beidz_darbu[3]), as.Date(t$sak_darbu[2]), units = "days"))
    days <- days1 + days2
    
    yt <- y2[v, ]
    yt$dienas <- days
  } else if (t$zinkod[1] == "25" && t$zinkod[2] == "51" && t$zinkod[3] == "50" && all(t$NDZ_sanemsanas_datums == t$NDZ_sanemsanas_datums[1])) {
    yt <- y2[v:(v+1), ]
    yt <- yt[yt$zinkod == "11", ]
  } else if (t$zinkod[1] == "50" && t$zinkod[2] == "25" && t$zinkod[3] == "51" && t$NDZ_sanemsanas_datums[2] == t$NDZ_sanemsanas_datums[3]) {
    days <- as.numeric(difftime(as.Date(t$beidz_darbu[1]), prev, units = "days")) - 1 #mīnus 1, jo 10-tajā viņš jau ir atvaļinājumā
    
    yt <- y2[v, ]
    yt$dienas <- days
  } else if (t$zinkod[1] == "11" && t$zinkod[2] == "50" &&
             t$zinkod[3] == "51" && all(diff(t$NDZ_sanemsanas_datums) != 0)) {
    days1 <- as.numeric(difftime(as.Date(t$beidz_darbu[2]), as.Date(t$sak_darbu[1]), units = "days")) 
    days2 <- as.numeric(difftime(as.Date(t$last_date[3]), as.Date(t$sak_darbu[3]), units = "days")) + 1 
    days <- days1 + days2 
    rm(days1, days2)
    
    yt <- y2[v, ]
    yt$dienas <- days
  } else if (t$zinkod[1] == "91" && t$zinkod[2] == "92" &&
            t$zinkod[3] == "40" && all(diff(t$NDZ_sanemsanas_datums) != 0)) {
    days1 <- as.numeric(difftime(as.Date(t$beidz_darbu[1]), prev, units = "days")) - 1 #mīnus 1, jo pēdējā datumā viņš jau ir atvaļinājumā
    days2 <- as.numeric(difftime(as.Date(t$beidz_darbuz[3]), as.Date(t$sak_darbu[2]), units = "days")) 
    days <- days1 + days2 
    rm(days1, days2)
    
    yt <- y2[v, ]
    yt$dienas <- days
  } else if (t$zinkod[1] == "25" && t$zinkod[2] == "11" &&
             t$zinkod[3] == "81" && all(diff(t$NDZ_sanemsanas_datums) != 0)) {
    days1 <- as.numeric(difftime(as.Date(t$beidz_darbu[1]), prev, units = "days"))
    days2 <- as.numeric(difftime(as.Date(t$last_date[3]), as.Date(t$sak_darbu[2]), units = "days")) 
    days <- days1 + days2 
    rm(days1, days2)
    
    yt <- y2[v, ]
    yt$dienas <- days
  } else if (t$zinkod[1] == "11" && t$zinkod[2] == "53" &&
             t$zinkod[3] == "81" && all(diff(t$NDZ_sanemsanas_datums) != 0)) {
    days <- as.numeric(difftime(as.Date(t$beidz_darbu[2]), as.Date(t$sak_darbu[1]), units = "days"))
    yt <- y2[v, ]
    yt$dienas <- days
  } else {
    stop("Trūkst izstrādes koda.")
  }
  
  yt$zinkod <- "combined"  #jo starpkodu dienu sarēķins
  return(yt)
  rm(days, yt)
}

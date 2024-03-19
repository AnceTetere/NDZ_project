starpkodi5 <- function(y2, t, prev) {

  if (t$zinkod[1] == "50") {
    yt <- starpkodi5_50(y2, t, prev)
  } else if (t$zinkod[1] == "91" && t$zinkod[2] == "92" && 
             t$NDZ_sanemsanas_datums[1] != t$NDZ_sanemsanas_datums[2] && 
             t$zinkod[3] == "91" && t$zinkod[4] == "92" && 
             t$NDZ_sanemsanas_datums[3] != t$NDZ_sanemsanas_datums[4] && 
             t$zinkod[5] == "40" && t$NDZ_sanemsanas_datums[4] <= t$NDZ_sanemsanas_datums[5]) {
    yt <- y2[v:(v+1), ]
    
    days1 <- as.numeric(difftime(as.Date(t$last_date[5]), as.Date(t$beidz[5]), units = "days")) + 1 #jo rēķinu atvaļinājumu apgriezti, priekš atņemšanas
    days2 <- yt$dienas[yt$zinkod == "91"]
    days <- days2 - days1
    rm(days1, days2)
    
    yt <- yt[1, ]
    yt$dienas <- days
  } else if (t$zinkod[1] == "11" && t$zinkod[2] == "50" && 
             t$NDZ_sanemsanas_datums[1] != t$NDZ_sanemsanas_datums[2] && 
             t$zinkod[3] == "51" && t$zinkod[4] == "50" && 
             t$NDZ_sanemsanas_datums[3] != t$NDZ_sanemsanas_datums[4] && 
             t$zinkod[5] == "51" && t$NDZ_sanemsanas_datums[4] <= t$NDZ_sanemsanas_datums[5]) {
    yt <- y2[v:(v+1), ]
    
    days1 <- yt$dienas[yt$zinkod == "11"]
    days2 <- as.numeric(difftime(as.Date(t$sak[1]), prev, units = "days")) - 1 #jo rēķinu atvaļinājumu apgriezti, priekš atņemšanas
    days <- days1 - days2
    rm(days1, days2)
    
    yt <- yt[1, ]
    yt$dienas <- days
  } else if (t$zinkod[1] == "91" && t$zinkod[2] == "92" && 
            t$NDZ_sanemsanas_datums[1] != t$NDZ_sanemsanas_datums[2] && 
            t$zinkod[3] == "91" && t$zinkod[4] == "92" && 
            t$NDZ_sanemsanas_datums[3] != t$NDZ_sanemsanas_datums[4] && 
            t$zinkod[5] == "25" && t$NDZ_sanemsanas_datums[4] != t$NDZ_sanemsanas_datums[5]) {
    days1 <- as.numeric(difftime(as.Date(t$beidz[1]), prev, units = "days")) - 1 #jo atvaļinājums apgriezti, Norādītajā datumā persona jau ir brīva.
    days2 <- as.numeric(difftime(as.Date(t$beidz[3]), as.Date(t$sak[2]), units = "days"))
    days3 <- as.numeric(difftime(as.Date(t$beidz[5]), as.Date(t$sak[4]), units = "days"))
    days <- days1 + days2 + days3
    rm(days1, days2, days3)
    
    yt <- y2[v, ]
    yt$dienas <- days
  } else if(t$zinkod[1] == "25" && t$zinkod[2] == "11" && 
            t$NDZ_sanemsanas_datums[1] != t$NDZ_sanemsanas_datums[2] && 
            t$NDZ_sanemsanas_datums[2] == t$NDZ_sanemsanas_datums[3] && 
            t$zinkod[3] == "81" && t$zinkod[4] == "25" && 
            t$NDZ_sanemsanas_datums[3] != t$NDZ_sanemsanas_datums[4] && 
            t$zinkod[5] == "82" && 
            t$NDZ_sanemsanas_datums[4] == t$NDZ_sanemsanas_datums[5]) {
    days1 <- as.numeric(difftime(as.Date(t$beidz[1]), prev, units = "days"))
    days2 <- as.numeric(difftime(as.Date(t$beidz[5]), as.Date(t$sak[3]), units = "days")) + 1 # jo darbs
    days <- days1 + days2
    rm(days1, days2)
    
    yt <- y2[v, ]
    yt$dienas <- days
  } else if(t$zinkod[1] == "91" && t$zinkod[2] == "92" && 
            t$zinkod[3] == "91" && t$zinkod[4] == "92" && 
            t$zinkod[5] == "21" && all(!diff(t$NDZ_sanemsanas_datums) == 0)) {
    days1 <- as.numeric(difftime(as.Date(t$beidz[1]), prev, units = "days")) - 1 # jo atvaļinājums
    days2 <- as.numeric(difftime(as.Date(t$beidz[3]), as.Date(t$sak[2]), units = "days"))
    days3 <- as.numeric(difftime(as.Date(t$beidz[5]), as.Date(t$sak[4]), units = "days"))
    days <- days1 + days2 + days3
    rm(days1, days2, days3)
    
    yt <- y2[v, ]
    yt$dienas <- days
  } else if(t$zinkod[1] == "11" && t$zinkod[2] == "81" && 
            t$zinkod[3] == "82" && t$zinkod[4] == "81" && 
            t$zinkod[5] == "82" && all(!diff(t$NDZ_sanemsanas_datums) == 0)) {
    days1 <- as.numeric(difftime(as.Date(t$beidz[3]), as.Date(t$sak[2]), units = "days")) + 1 #jo darbs
    days2 <- as.numeric(difftime(as.Date(t$beidz[5]), as.Date(t$sak[4]), units = "days")) + 1
    days <- days1 + days2
    rm(days1, days2)
    
    yt <- y2[v, ]
    yt$dienas <- days
  } else if(t$zinkod[1] == "11" && t$zinkod[2] == "91" && 
           t$zinkod[3] == "92" && t$zinkod[4] == "91" && 
           t$zinkod[5] == "92" && all(!diff(t$NDZ_sanemsanas_datums) == 0)) {
    days1 <- as.numeric(difftime(as.Date(t$beidz[2]), as.Date(t$sak[1]), units = "days"))
    days2 <- as.numeric(difftime(as.Date(t$beidz[4]), as.Date(t$sak[3]), units = "days"))
    days3 <- as.numeric(difftime(as.Date(t$last_date[5]), as.Date(t$sak[5]), units = "days")) + 1 #jo darbs
    days <- days1 + days2 + days3
    rm(days1, days2, days3)
    
    yt <- y2[v, ]
    yt$dienas <- days
  } else if(t$zinkod[1] == "11" && t$zinkod[2] == "81" && 
           t$zinkod[3] == "25" && t$zinkod[4] == "82" && 
           t$zinkod[5] == "11" && t$NDZ_sanemsanas_datums[1] == t$NDZ_sanemsanas_datums[2] &&
           t$NDZ_sanemsanas_datums[3] == t$NDZ_sanemsanas_datums[4]) {
    yt <- y2[v:(v+1), ]
    yt <- yt[yt$zinkod == "11", ]
  } else if(t$zinkod[1] == "25" && t$zinkod[2] == "25" && 
            t$zinkod[3] == "11" && t$zinkod[4] == "81" && 
            t$zinkod[5] == "82" && t$NDZ_sanemsanas_datums[1] != t$NDZ_sanemsanas_datums[2] &&
            all(diff(t$NDZ_sanemsanas_datums[2:5]) == 0)) {
    days1 <- as.numeric(difftime(t$beidz[1], prev, units = "days")) 
    days2 <- 1 #uz vienu dienu paņemts
    days <- days1 + days2
    rm(days1, days2)
    
    yt <- y2[v, ]
    yt$dienas <- days
  } else if(t$zinkod[1] == "25" && t$zinkod[2] == "26" && 
            t$zinkod[3] == "11" && t$zinkod[4] == "11" && 
            t$zinkod[5] == "81" && 
            t$NDZ_sanemsanas_datums[1] != t$NDZ_sanemsanas_datums[2] &&
            t$NDZ_sanemsanas_datums[2] == t$NDZ_sanemsanas_datums[3] &&
            t$NDZ_sanemsanas_datums[4] == t$NDZ_sanemsanas_datums[5]) {
    days1 <- as.numeric(difftime(t$beidz[1], prev, units = "days")) 
    days2 <- 0 #darba neuzsākšana
    days3 <- as.numeric(difftime(t$last_date[5], t$sak[5], units = "days"))
    
    days <- days1 + days2 + days3
    rm(days1, days2, days3)
    
    yt <- y2[v, ]
    yt$dienas <- days
  } else if(t$zinkod[1] == "25" && t$zinkod[2] == "11" && 
           t$zinkod[3] == "81" && t$zinkod[4] == "82" && 
           t$zinkod[5] == "11" && 
           all(diff(t$NDZ_sanemsanas_datums[1:4]) == 0) &&
           t$NDZ_sanemsanas_datums[4] != t$NDZ_sanemsanas_datums[5]) {
    days1 <- as.numeric(difftime(t$beidz[4], t$sak[3], units = "days")) + 1 #jo darbs
    days2 <- as.numeric(difftime(t$last_date[5], t$sak[5], units = "days")) +1 #jo darbs 
    days <- sum(days1, days2)
    rm(days1, days2)
    
    yt <- y2[v, ]
    yt$dienas <- days
  } else {
    stop("Starpkodi5: iztrūkst apstrādes koda.")
  }
  
  yt$zinkod <- "combined"  #jo starpkodu dienu sarēķins
  return(yt)
  rm(days, yt)
}

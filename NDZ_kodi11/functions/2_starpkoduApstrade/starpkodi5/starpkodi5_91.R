starpkodi5_91 <- function(y2, t, prev, v) {
  
  if (t$zinkod[2] == "92" && t$NDZ_sanemsanas_datums[1] != t$NDZ_sanemsanas_datums[2] && 
      t$zinkod[3] == "91" && t$zinkod[4] == "92" && t$NDZ_sanemsanas_datums[3] != t$NDZ_sanemsanas_datums[4] && 
      t$zinkod[5] == "40" && t$NDZ_sanemsanas_datums[4] <= t$NDZ_sanemsanas_datums[5]) {
    yt <- y2[v:(v+1), ]
    
    days1 <- as.numeric(difftime(t$last_date[5], t$beidz[5], units = "days")) + 1 #jo rēķinu atvaļinājumu apgriezti, priekš atņemšanas
    days2 <- yt$dienas[yt$zinkod == "91"]
    days <- days2 - days1
    rm(days1, days2)
    
    yt <- yt[1, ]
    yt$dienas <- days
  } else if (t$zinkod[2] == "92" && t$zinkod[3] == "91" && t$zinkod[4] == "92" && t$zinkod[5] == "25") {
    days1 <- as.numeric(difftime(t$beidz[1], prev, units = "days")) - 1  
    days2 <- as.numeric(difftime(t$beidz[3], t$sak[2], units = "days"))
    days3 <- as.numeric(difftime(t$beidz[5], t$sak[4], units = "days"))
    
    yt <- y2[v, ]
    yt$dienas <- sum(days1, days2, days3)
    rm(days1, days2, days3)
  }  else if(t$zinkod[2] == "92" && t$zinkod[3] == "91" && t$zinkod[4] == "92" && 
             t$zinkod[5] == "21" && all(diff(t$NDZ_sanemsanas_datums) != 0)) {
    days1 <- as.numeric(difftime(t$beidz[1], prev, units = "days")) - 1 
    days2 <- as.numeric(difftime(t$beidz[3], t$sak[2], units = "days"))
    days3 <- as.numeric(difftime(t$beidz[5], t$sak[4], units = "days"))
    
    yt <- y2[v, ]
    yt$dienas <- sum(days1, days2, days3)
    rm(days1, days2, days3)
  } else if(t$zinkod[2] == "91" && t$zinkod[3] == "92" && t$zinkod[4] == "92" && 
            t$zinkod[5] == "25" && all(diff(t$NDZ_sanemsanas_datums[2:3]) == 0) && all(diff(t$NDZ_sanemsanas_datums[1:2]) != 0) && all(diff(t$NDZ_sanemsanas_datums[3:4]) != 0)) {
    days1 <- as.numeric(difftime(t$beidz[1], prev, units = "days")) - 1
    days2 <- as.numeric(difftime(t$beidz[5], t$sak[4], units = "days")) + 1 

    yt <- y2[v, ]
    yt$dienas <- sum(days1, days2)
    rm(days1, days2)
  } else if(t$zinkod[3] == "91" && all(t$zinkod[c(2,4)] == "92") && t$zinkod[5] == "50" && all(diff(t$NDZ_sanemsanas_datums) != 0)) {

    days1 <- as.numeric(difftime(t$beidz[1], prev, units = "days")) - 1 
    days2 <- as.numeric(difftime(t$beidz[3], t$sak[2], units = "days"))
    days3 <- as.numeric(difftime(t$beidz[5], t$sak[4], units = "days"))
    
    yt <- y2[v, ]
    yt$dienas <- sum(days1, days2, days3)
    rm(days1, days2, days3)
  } else if(t$zinkod[2] == "51" && all(t$zinkod[c(3,5)] == "92") && t$zinkod[4] == "91" && 
            all(diff(t$NDZ_sanemsanas_datums[2:5]) != 0) && diff(t$NDZ_sanemsanas_datums[1:2]) == 0) {
    days1 <- as.numeric(difftime(t$beidz[4], t$sak[3], units = "days"))
    days2 <- as.numeric(difftime(t$last_date[5], t$sak[5], units = "days")) + 1 
    
    yt <- y2[v, ]
    yt$dienas <- sum(days1, days2)
    rm(days1, days2)
  } else {
    stop("Starpkodi5_91: Trūkst izstrādes koda.")
  }
  
  if(is.na(yt$PS_code[1])) {stop("Dienas NA.")}
  return(yt) 
}

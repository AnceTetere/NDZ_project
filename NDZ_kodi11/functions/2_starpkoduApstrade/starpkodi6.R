starpkodi6 <- function(y2, t, prev, v) {
  
  if (t$zinkod[1] == "11") {
    yt <- starpkodi6_11(y2, t, prev, v)
  } else if (t$zinkod[1] == "26" && 
             t$zinkod[2] == "82" && 
             t$zinkod[3] == "11" && 
             t$zinkod[4] == "25" && 
             t$zinkod[5] == "81" && 
             t$zinkod[6] == "82" && 
             all(diff(t$NDZ_sanemsanas_datums[3:6]) == 0) &&
             t$NDZ_sanemsanas_datums[1] ==t$NDZ_sanemsanas_datums[2]) {
    yt <- y2[v, ]
    yt$dienas <- 1
  } else if (t$zinkod[1] == "91" && 
             t$zinkod[2] == "92" && 
             t$zinkod[3] == "91" && 
             t$zinkod[4] == "92" && 
             t$zinkod[5] == "91" && 
             t$zinkod[6] == "25" && 
             all(!diff(t$NDZ_sanemsanas_datums) == 0)) {
    days1 <- as.numeric(difftime(as.Date(t$beidz[1]), prev, units = "days")) - 1 # jo atvaļinājums
    days2 <- as.numeric(difftime(as.Date(t$beidz[3]), as.Date(t$sak[2]), units = "days"))
    days3 <- as.numeric(difftime(as.Date(t$beidz[5]), as.Date(t$sak[4]), units = "days")) + 1 # šis vieninieks ir tā viena diena no kodiem 81 un 82
    days <- days1 + days2 + days3
    rm(days1, days2, days3)
    
    yt <- y2[v, ]
    yt$dienas <- days
  } else if (t$zinkod[1] == "51" && 
             t$zinkod[2] == "25" && 
             t$zinkod[3] == "11" && 
             t$zinkod[4] == "50" && 
             t$zinkod[5] == "25" && 
             t$zinkod[6] == "51" && 
            all(diff(t$NDZ_sanemsanas_datums[1:5]) != 0) && all(diff(t$NDZ_sanemsanas_datums[5:6]) == 0)) {
    days1 <- as.numeric(difftime(t$beidz[2], t$sak[1], units = "days")) + 1 #jo atlaišana
    days2 <- as.numeric(difftime(t$beidz[4], t$sak[3], units = "days"))
    days3 <- 1 #atgriezās, lai tiktu atlaists - pieņemu, ka tā, bet varu kļūdīties.
    
    yt <- y2[v, ]
    yt$dienas <- sum(days1, days2, days3)
    rm(days1, days2, days3)
  } else if (t$zinkod[1] == "40" && 
             t$zinkod[2] == "50" && 
             t$zinkod[3] == "41" && 
             t$zinkod[4] == "51" && 
             t$zinkod[5] == "91" && 
             t$zinkod[6] == "92" && 
             all(diff(t$NDZ_sanemsanas_datums[1:2]) == 0) && all(diff(t$NDZ_sanemsanas_datums[3:4]) == 0) &&
             all(diff(t$NDZ_sanemsanas_datums[4:6]) != 0) && all(diff(t$NDZ_sanemsanas_datums[2:3]) != 0)) {
    yt <- y2[v:(v+1), ]
    yt <- yt[yt$zinkod == "40", ]

  } else if (t$zinkod[1] == "41" && 
             t$zinkod[2] == "50" && 
             t$zinkod[3] == "51" && 
             t$zinkod[4] == "40" && 
             t$zinkod[5] == "41" && 
             t$zinkod[6] == "50" && 
             all(diff(t$NDZ_sanemsanas_datums[1:2]) == 0) && all(diff(t$NDZ_sanemsanas_datums[2:5]) != 0) &&
             all(diff(t$NDZ_sanemsanas_datums[5:6]) == 0)) {
    yt <- y2[v, ]
    yt$dienas <- as.numeric(difftime(t$beidz[4], t$sak[3], units = "days"))
  } else if (t$zinkod[1] == "50" && t$zinkod[2] == "51" && 
             t$zinkod[3] == "50" && t$zinkod[4] == "91" && 
             t$zinkod[5] == "51" && t$zinkod[6] == "92" && 
             all(diff(t$NDZ_sanemsanas_datums[4:5]) == 0) && all(diff(t$NDZ_sanemsanas_datums[5:6]) != 0) &&
             all(diff(t$NDZ_sanemsanas_datums[1:4]) != 0)) {
    days1 <- as.numeric(difftime(t$beidz[1], prev, units = "days")) - 1 #jo atvaļinājums
    days2 <- as.numeric(difftime(t$beidz[3], t$sak[2], units = "days"))
    days3 <- as.numeric(difftime(t$last_date[6], t$sak[6], units = "days")) + 1 #jo darbs
    
    yt <- y2[v, ]
    yt$dienas <- sum(days1, days2, days3)
    rm(days1, days2, days3)
  } else if (t$zinkod[1] == "53" && t$zinkod[2] == "53" && 
             t$zinkod[3] == "54" && t$zinkod[4] == "54" && 
             t$zinkod[5] == "53" && t$zinkod[6] == "21" && 
             all(diff(t$NDZ_sanemsanas_datums[2:3]) == 0) && all(diff(t$NDZ_sanemsanas_datums[1:2]) != 0) &&
             all(diff(t$NDZ_sanemsanas_datums[3:6]) != 0)) {
    days1 <- as.numeric(difftime(t$beidz[1], prev, units = "days")) - 1 #jo dīkstāve
    days2 <- as.numeric(difftime(t$beidz[5], t$sak[4], units = "days"))
    
    yt <- y2[v, ]
    yt$dienas <- sum(days1, days2)
    rm(days1, days2)
  } else if (all(t$zinkod[c(1,3,5)] == "51") && all(t$zinkod[c(2,4)] == "50") && t$zinkod[6] == "25" && 
             all(diff(t$NDZ_sanemsanas_datums) != 0)) {
    days1 <- as.numeric(difftime(t$beidz[2], t$sak[1], units = "days"))
    days2 <- as.numeric(difftime(t$beidz[4], t$sak[3], units = "days"))
    days3 <- as.numeric(difftime(t$beidz[6], t$sak[5], units = "days")) + 1 #jo atlaišana
    
    yt <- y2[v, ]
    yt$dienas <- sum(days1, days2, days3)
    rm(days1, days2, days3)
  } else {
    stop("Starpkodi6 iztrūkst apstrādes koda.")
  }
  
  if(is.na(yt$PS_code[1])) {stop("Dienas NA.")}
  yt$zinkod <- "combined"  #jo starpkodu dienu sarēķins
  return(yt)
}


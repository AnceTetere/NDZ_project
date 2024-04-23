starpkodi8 <- function(y2, t, prev, v) {

    if (t$zinkod[1] == "11" && t$zinkod[2] == "50" && 
        t$zinkod[3] == "51" && t$zinkod[4] == "50" && 
        t$zinkod[5] == "51" && t$zinkod[6] == "50" && 
        t$zinkod[7] == "51" && t$zinkod[8] == "50" && all(!diff(t$NDZ_sanemsanas_datums) == 0)) {
    days1 <- as.numeric(difftime(as.Date(t$beidz[2]), as.Date(t$sak[1]), units = "days"))
    days2 <- as.numeric(difftime(as.Date(t$beidz[4]), as.Date(t$sak[3]), units = "days"))
    days3 <- as.numeric(difftime(as.Date(t$beidz[6]), as.Date(t$sak[5]), units = "days"))
    days4 <- as.numeric(difftime(as.Date(t$beidz[8]), as.Date(t$sak[7]), units = "days"))
    
    yt <- y2[v, ]
    yt$dienas <- sum(days1, days2, days3, days4)
    rm(days1, days2, days3, days4)
  } else if (t$zinkod[1] == "50" && t$zinkod[2] == "51" && 
             t$zinkod[3] == "50" && t$zinkod[4] == "51" && 
             t$zinkod[5] == "50" && t$zinkod[6] == "51" && 
             t$zinkod[7] == "50" && t$zinkod[8] == "25" && all(!diff(t$NDZ_sanemsanas_datums) == 0)) {
    days1 <- as.numeric(difftime(t$beidz[1], prev, units = "days")) - 1 #jo atvaļinājums
    days2 <- as.numeric(difftime(as.Date(t$beidz[3]), as.Date(t$sak[2]), units = "days"))
    days3 <- as.numeric(difftime(as.Date(t$beidz[5]), as.Date(t$sak[4]), units = "days"))
    days4 <- as.numeric(difftime(as.Date(t$beidz[7]), as.Date(t$sak[6]), units = "days")) 
    
    yt <- y2[v, ]
    yt$dienas <- sum(days1, days2, days3, days4)
    rm(days1, days2, days3, days4)
  } else if (t$zinkod[1] == "21" && t$zinkod[2] == "11" && 
             t$zinkod[3] == "50" && t$zinkod[4] == "51" && 
             t$zinkod[5] == "50" && t$zinkod[6] == "51" && 
             t$zinkod[7] == "50" && t$zinkod[8] == "51" && all(diff(t$NDZ_sanemsanas_datums[4:5]) == 0) &&
             all(!diff(t$NDZ_sanemsanas_datums[1:3]) == 0) && all(!diff(t$NDZ_sanemsanas_datums[5:8]) == 0)) {
    days1 <- as.numeric(difftime(t$beidz[1], prev, units = "days"))
    days2 <- as.numeric(difftime(t$beidz[3], t$sak[2], units = "days"))
    days3 <- as.numeric(difftime(t$beidz[5], t$sak[4], units = "days"))
    days4 <- as.numeric(difftime(t$beidz[7], t$sak[6], units = "days"))
    days5 <- as.numeric(difftime(t$last_date[8], t$sak[8], units = "days"))
    
    yt <- y2[v, ]
    yt$dienas <- sum(days1, days2, days3, days4, days5)
    rm(days1, days2, days3, days4, days5)
  } else if (t$zinkod[1] == "11" && t$zinkod[2] == "53" && 
             t$zinkod[3] == "54" && t$zinkod[4] == "53" && 
             t$zinkod[5] == "54" && t$zinkod[6] == "53" && 
             t$zinkod[7] == "54" && t$zinkod[8] == "53" && all(diff(t$NDZ_sanemsanas_datums[3:4]) == 0) &&
             all(diff(t$NDZ_sanemsanas_datums[7:8]) == 0) && all(diff(t$NDZ_sanemsanas_datums[1:3]) != 0) &&
             all(diff(t$NDZ_sanemsanas_datums[5:7]) != 0)) {
    days1 <- as.numeric(difftime(t$beidz[2], t$sak[1], units = "days"))
    days2 <- as.numeric(difftime(t$beidz[4], t$sak[3], units = "days"))
    days3 <- as.numeric(difftime(t$beidz[6], t$sak[5], units = "days"))
    days4 <- as.numeric(difftime(t$beidz[8], t$sak[7], units = "days"))
    
    yt <- y2[v, ]
    yt$dienas <- sum(days1, days2, days3, days4)
    rm(days1, days2, days3, days4)
  } else if (t$zinkod[1] == "25" && t$zinkod[2] == "11" && 
             t$zinkod[3] == "11" && t$zinkod[4] == "50" && 
             t$zinkod[5] == "25" && t$zinkod[6] == "51" && 
             t$zinkod[7] == "25" && t$zinkod[8] == "11" && 
             all(sapply(seq(1, nrow(t), by = 2), function(i) all(diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0))) &&
             t$PS_code[1] == '______' && t$nmrkod[1] == '_______') {
    days1 <- as.numeric(difftime(t$beidz[1], t$sak[2], units = "days")) + 1 
    days2 <- 0 
    days3 <- as.numeric(difftime(t$beidz[7], t$sak[8], units = "days")) + 1 
    
    yt <- y2[v, ]
    yt$dienas <- sum(days1, days2, days3)
    rm(days1, days2, days3)
  } else {
    stop("Starpkodi8 iztrūkst apstrādes koda.")
  }
  
  if(is.na(yt$PS_code[1])) {stop("Dienas NA.")}
  yt$zinkod <- "combined"  #jo starpkodu dienu sarēķins
  return(yt)
}

starpkodi7 <- function(y2, t, prev, v) {
  
  if (t$zinkod[1] == "11" && t$zinkod[2] == "91" && t$zinkod[3] == "92" && t$zinkod[4] == "91" && t$zinkod[5] == "92" && t$zinkod[6] == "91" && t$zinkod[7] == "92" && any(!diff(t$NDZ_sanemsanas_datums) == 0)) {
    
    days1 <- as.numeric(difftime(as.Date(t$beidz[2]), as.Date(t$sak[1]), units = "days"))
    days2 <- as.numeric(difftime(as.Date(t$beidz[4]), as.Date(t$sak[3]), units = "days"))
    days3 <- as.numeric(difftime(as.Date(t$beidz[6]), as.Date(t$sak[5]), units = "days"))
    days4 <- as.numeric(difftime(as.Date(t$last_date[7]), as.Date(t$sak[7]), units = "days")) + 1
    
    days <- days1 + days2 + days3 + days4
    rm(days1, days2, days3, days4)
    
    yt <- y2[v, ]
    yt$dienas <- days
  } else if (t$zinkod[1] == "91" && 
             t$zinkod[2] == "92" && 
             t$zinkod[3] == "40" && 
             t$zinkod[4] == "40" && 
             t$zinkod[5] == "41" && 
             t$zinkod[6] == "40" && 
             t$zinkod[7] == "41" && 
             t$NDZ_sanemsanas_datums[1] != t$NDZ_sanemsanas_datums[2] &&
             t$NDZ_sanemsanas_datums[3] != t$NDZ_sanemsanas_datums[4] &&
             t$NDZ_sanemsanas_datums[4] == t$NDZ_sanemsanas_datums[5] &&
             t$NDZ_sanemsanas_datums[6] == t$NDZ_sanemsanas_datums[7]) {
    
    days1 <- as.numeric(difftime(as.Date(t$beidz[1]), prev, units = "days")) - 1 #jo atvaļinājums 
    days2 <- as.numeric(difftime(as.Date(t$beidz[3]), as.Date(t$sak[2]), units = "days"))
    days3 <- as.numeric(difftime(as.Date(t$beidz[4]), as.Date(t$sak[5]), units = "days")) 
    days4 <- as.numeric(difftime(as.Date(t$beidz[6]), as.Date(t$sak[7]), units = "days"))
    
    days <- days1 + days2 + days3 + days4
    rm(days1, days2, days3, days4)
    
    yt <- y2[v, ]
    yt$dienas <- days
  } else if (t$zinkod[1] == "50" && 
             t$zinkod[2] == "51" && 
             t$zinkod[3] == "50" && 
             t$zinkod[4] == "51" && 
             t$zinkod[5] == "50" && 
             t$zinkod[6] == "25" && 
             t$zinkod[7] == "51" && 
             t$NDZ_sanemsanas_datums[6] == t$NDZ_sanemsanas_datums[7] &&
             all(!diff(t$NDZ_sanemsanas_datums[1:6]) == 0)) {
    
    days1 <- as.numeric(difftime(as.Date(t$beidz[1]), prev, units = "days")) - 1 
    days2 <- as.numeric(difftime(as.Date(t$beidz[3]), as.Date(t$sak[2]), units = "days"))
    days3 <- as.numeric(difftime(as.Date(t$beidz[5]), as.Date(t$sak[4]), units = "days"))
    
    days <- days1 + days2 + days3
    rm(days1, days2, days3)
    
    yt <- y2[v, ]
    yt$dienas <- days
  } else if (t$zinkod[1] == "50" && 
             t$zinkod[2] == "51" && 
             t$zinkod[3] == "50" && 
             t$zinkod[4] == "51" && 
             t$zinkod[5] == "50" && 
             t$zinkod[6] == "51" && 
             t$zinkod[7] %in% c("21", "25") && 
             all(!diff(t$NDZ_sanemsanas_datums) == 0)) {
    days1 <- as.numeric(difftime(t$beidz[1], prev, units = "days")) - 1
    days2 <- as.numeric(difftime(t$beidz[3], t$sak[2], units = "days"))
    days3 <- as.numeric(difftime(t$beidz[5], t$sak[4], units = "days")) 
    days4 <- as.numeric(difftime(t$beidz[7], t$sak[6], units = "days")) + 1
    days <- days1 + days2 + days3 + days4
    rm(days1, days2, days3, days4)
    
    yt <- y2[v, ]
    yt$dienas <- days
  } else if (t$zinkod[1] == "11" && 
             t$zinkod[2] == "50" && 
             t$zinkod[3] == "51" && 
             t$zinkod[4] == "50" && 
             t$zinkod[5] == "51" && 
             t$zinkod[6] == "50" && 
             t$zinkod[7] %in% c("21", "25") && 
             all(!diff(t$NDZ_sanemsanas_datums) == 0)) {

    days1 <- as.numeric(difftime(t$beidz[2], t$sak[1], units = "days"))
    days2 <- as.numeric(difftime(t$beidz[4], t$sak[3], units = "days"))
    days3 <- as.numeric(difftime(t$beidz[6], t$sak[5], units = "days")) 
    
    yt <- y2[v, ]
    yt$dienas <- sum(days1, days2, days3) 
    rm(days1, days2, days3)
  } else if (t$zinkod[1] == "91" && 
             t$zinkod[2] == "92" && 
             t$zinkod[3] == "91" && 
             t$zinkod[4] == "92" && 
             t$zinkod[5] == "91" && 
             t$zinkod[6] == "92" && 
             t$zinkod[7] %in% c("21", "25") && 
             all(!diff(t$NDZ_sanemsanas_datums) == 0)) {

    days1 <- as.numeric(difftime(t$beidz[1], prev, units = "days")) - 1 
    days2 <- as.numeric(difftime(t$beidz[3], t$sak[2], units = "days"))
    days3 <- as.numeric(difftime(t$beidz[5], t$sak[4], units = "days")) 
    days4 <- as.numeric(difftime(t$beidz[7], t$sak[6], units = "days")) + 1 
    
    yt <- y2[v, ]
    yt$dienas <- sum(days1, days2, days3, days4) 
    rm(days1, days2, days3, days4)
  } else if (t$zinkod[1] == "50" && 
             t$zinkod[2] == "51" && 
             t$zinkod[3] == "25" && 
             t$zinkod[4] == "11" && 
             t$zinkod[5] == "50" && 
             t$zinkod[6] == "25" && 
             t$zinkod[7] == "51" && 
             all(diff(t$NDZ_sanemsanas_datums[1:4]) != 0) &&
             all(diff(t$NDZ_sanemsanas_datums[4:5]) == 0) &&
             all(diff(t$NDZ_sanemsanas_datums[6:7]) == 0)) {

    days1 <- as.numeric(difftime(t$beidz[1], prev, units = "days")) - 1 
    days2 <- as.numeric(difftime(t$beidz[3], t$sak[2], units = "days")) + 1 
    yt <- y2[v, ]
    yt$dienas <- sum(days1, days2)
    rm(days1, days2)
  } else if (t$zinkod[1] == "91" && 
             t$zinkod[2] == "92" && 
             t$zinkod[3] == "91" && 
             t$zinkod[4] == "92" && 
             t$zinkod[5] == "91" && 
             t$zinkod[6] == "25" && 
             t$zinkod[7] == "92" && 
             all(diff(t$NDZ_sanemsanas_datums[1:6]) != 0) &&
             all(diff(t$NDZ_sanemsanas_datums[6:7]) == 0)) {

    days1 <- as.numeric(difftime(t$beidz[1], prev, units = "days")) - 1 
    days2 <- as.numeric(difftime(t$beidz[3], t$sak[2], units = "days")) + 1
    days3 <- as.numeric(difftime(t$beidz[5], t$sak[4], units = "days")) + 1 
    yt <- y2[v, ]
    yt$dienas <- sum(days1, days2, days3)
    rm(days1, days2, days3)
  } else {
    stop("Starpkodi7 iztrūkst apstrādes koda.")
  }
  
  if(is.na(yt$PS_code[1])) {stop("Dienas NA.")}
  yt$zinkod <- "combined"  #jo starpkodu dienu sarēķins
  return(yt)
}

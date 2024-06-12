tripletkodi4 <- function(y3, t, prev, v) {
  
  if (t$zinkod[1] == "41" && t$zinkod[2] == "50" && 
             t$zinkod[3] == "25" && t$zinkod[4] == "51" && 
             t$NDZ_sanemsanas_datums[1] == t$NDZ_sanemsanas_datums[2] &&
             t$NDZ_sanemsanas_datums[2] != t$NDZ_sanemsanas_datums[3] &&
             t$NDZ_sanemsanas_datums[3] == t$NDZ_sanemsanas_datums[4]) {
    yt <- y3[v, ]
    yt$dienas <- 0
  } else if (t$zinkod[1] == "41" && t$zinkod[2] == "50" && 
       t$zinkod[3] == "51" && t$zinkod[4] == "25" && all(diff(t$NDZ_sanemsanas_datums) != 0)) {
    days1 <- as.numeric(difftime(t$beidz[2], t$sak[1], units = "days"))
    days2 <- as.numeric(difftime(t$beidz[4], t$sak[3], units = "days"))
    
    yt <- y3[v, ]
    yt$dienas <- sum(days1, days2)
    rm(days1, days2)
  } else if (t$zinkod[1] == "50" && t$zinkod[2] == "53" && 
             t$zinkod[3] == "25" && t$zinkod[4] == "54" && 
             diff(t$NDZ_sanemsanas_datums[1:2]) == 0 && all(diff(t$NDZ_sanemsanas_datums[2:4]) != 0)) {
    yt <- y3[v, ]
    yt$dienas <- as.numeric(difftime(t$beidz[1], prev, units = "days")) - 1
  } else if (t$zinkod[1] == "40" && t$zinkod[2] == "50" && 
             t$zinkod[3] == "41" && t$zinkod[4] == "25" && 
             diff(t$NDZ_sanemsanas_datums[1:2]) == 0 && all(diff(t$NDZ_sanemsanas_datums[2:4]) != 0)) {
    days1 <- as.numeric(difftime(t$beidz[1], prev, units = "days")) - 1
    days2 <- as.numeric(difftime(t$beidz[4], t$sak[3], units = "days")) + 1
    
    yt <- y3[v, ]
    yt$dienas <- sum(days1, days2)
    rm(days1, days2)
  } else if (t$zinkod[1] == "50" && t$zinkod[2] == "51" && t$zinkod[3] == "25" && t$zinkod[4] == "54" && 
             all(sapply(seq(1,4,by=2), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) && 
             diff(t$NDZ_sanemsanas_datums[2:3]) != 0 && t$PS_code[1] == '__________' && t$NM_code[1] == '___________') {
    days1 <- as.numeric(difftime(t$beidz[1], prev, units = "days")) - 1
    days2 <- as.numeric(difftime(t$beidz[3], t$sak[2], units = "days"))
    
    yt <- y3[v, ]
    yt$dienas <- sum(days1, days2)
    rm(days1, days2)
  } else if (t$zinkod[1] == "91" && t$zinkod[2] == "51" && t$zinkod[3] == "92" && t$zinkod[4] == "25" && 
             diff(t$NDZ_sanemsanas_datums[1:2]) == 0 && 
             all(diff(t$NDZ_sanemsanas_datums[2:4]) != 0)) {
    days1 <- as.numeric(difftime(t$beidz[1], t$sak[2], units = "days"))
    days2 <- as.numeric(difftime(t$beidz[4], t$sak[3], units = "days")) + 1
    
    yt <- y3[v, ]
    yt$dienas <- sum(days1, days2)
    rm(days1, days2)
  } else if (t$zinkod[1] == "50" && t$zinkod[2] == "53" && t$zinkod[3] == "21" && t$zinkod[4] == "54" && 
             all(sapply(seq(1,4,by=2), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) && 
             diff(t$NDZ_sanemsanas_datums[2:3]) != 0) {
    yt <- y3[v, ]
    yt$dienas <- as.numeric(difftime(t$beidz[1], prev, units = "days")) - 1
  } else if (t$zinkod[1] == "51" && t$zinkod[2] == "53" && t$zinkod[3] == "21" && t$zinkod[4] == "54" && 
             all(diff(t$NDZ_sanemsanas_datums[1:3]) != 0) && diff(t$NDZ_sanemsanas_datums[3:4]) == 0) {
    yt <- y3[v, ]
    yt$dienas <- as.numeric(difftime(t$beidz[2], t$sak[1], units = "days"))
  } else {
    stop("Tripletkodi4 iztrūkst apstrādes koda.")
  }
  
  if(is.na(yt$PS_code[1])) {stop("Dienas NA.")}
  yt$zinkod <- "combined"  #jo tripletkodu dienu sarēķins
  return(yt)
}

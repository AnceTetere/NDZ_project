starpkodi5_11_50 <- function(y2, t, prev, v) {
  if (t$zinkod[4] == "50" && all(t$zinkod[c(3,5)] == "51") && all(diff(t$NDZ_sanemsanas_datums[2:5]) != 0)) {
    yt <- y2[v, ]
    yt$dienas <- sum(sapply(seq(1,4,by=2), function(i) as.numeric(difftime(t$beidz[i+1], t$sak[i], units = "days"))),
                     as.numeric(difftime(t$last_date[5], t$sak[5], units = "days")) + 1)
  } else if(t$zinkod[3] == "51" && t$zinkod[4] == "50" && t$zinkod[5] == "21" && 
            all(diff(t$NDZ_sanemsanas_datums) != 0)) {
    days1 <- as.numeric(difftime(t$beidz[2], t$sak[1], units = "days"))
    days2 <- as.numeric(difftime(t$beidz[4], t$sak[3], units = "days")) 
    
    yt <- y2[v, ]
    yt$dienas <- sum(days1, days2)
    rm(days1, days2)
  } else if(all(t$zinkod[c(3,5)] == "51") && t$zinkod[4] == "50" && 
                all(diff(t$NDZ_sanemsanas_datums[2:5]) != 0) && diff(t$NDZ_sanemsanas_datums[1:2] == 0)) {
    days1 <- as.numeric(difftime(t$beidz[4], t$sak[3], units = "days"))
    days2 <- as.numeric(difftime(t$last_date[5], t$sak[5], units = "days")) + 1 
    
    yt <- y2[v, ]
    yt$dienas <- sum(days1, days2)
    rm(days1, days2)
  } else if(t$zinkod[3] == "50" && all(t$zinkod[4:5] == "51") && 
            all(sapply(c(2,4), function(i) all(diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0))) &&
            all(sapply(c(1,3), function(i) all(diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0)))) {
    days1 <- as.numeric(difftime(t$beidz[2], t$sak[1], units = "days"))
    days2 <- as.numeric(difftime(t$last_date[5], t$sak[5], units = "days")) + 1 
    
    yt <- y2[v, ]
    yt$dienas <- sum(days1, days2)
    rm(days1, days2)
  } else if (t$zinkod[3] == "51" && t$zinkod[4] == "21" && t$zinkod[5] == "50" && 
             all(diff(t$NDZ_sanemsanas_datums[1:4]) != 0) && all(diff(t$NDZ_sanemsanas_datums[4:5]) == 0)) {
    days1 <- as.numeric(difftime(t$beidz[2], t$sak[1], units = "days"))
    days2 <- as.numeric(difftime(t$beidz[4], t$sak[3], units = "days"))
    
    yt <- y2[v, ]
    yt$dienas <- sum(days1, days2)
    rm(days1, days2)
  } else if (t$zinkod[3] == "50" && t$zinkod[4] == "51" && t$zinkod[5] == "25" && all(diff(t$NDZ_sanemsanas_datums) != 0)) {
    days1 <- as.numeric(difftime(t$beidz[3], t$sak[1], units = "days"))
    days2 <- as.numeric(difftime(t$beidz[5], t$sak[4], units = "days")) + 1
    
    yt <- y2[v, ]
    yt$dienas <- sum(days1, days2)
    rm(days1, days2)
  } else if (t$zinkod[3] == "25" && all(t$zinkod[4:5] == "51") && diff(t$NDZ_sanemsanas_datums[3:4]) == 0 &&
             all(sapply(c(1,2,4), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
    yt <- y2[v, ]
    yt$dienas <- as.numeric(difftime(t$beidz[2], t$sak[1], units = "days"))
  } else if (t$zinkod[3] == "11" && t$zinkod[4] == "50" && t$zinkod[5] == "51" && 
             all(sapply(seq(2,5,by=2), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0)) &&
             all(sapply(seq(1,4,by=2), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0))) {
    yt <- y2[v, ]
    yt$dienas <- as.numeric(difftime(t$last_date[5], t$sak[5], units = "days")) + 1
  } else if (t$zinkod[3] == "25" && t$zinkod[4] == "50" && t$zinkod[5] == "51" && 
             diff(t$NDZ_sanemsanas_datums[2:3]) != 0 &&
             all(sapply(c(1,3,4), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0))) {
    yt <- y2[v, ]
    yt$dienas <- 0
  } else {
    stop("Starpkodi5_11_50: Trūkst izstrādes koda.")
  }
  
  if(is.na(yt$PS_code[1])) {stop("Dienas NA.")}
  return(yt)
}

starpkodi6_50 <- function(y2, t, prev, v) {
  
  if (all(t$zinkod[c(2, 5)] == "51") && t$zinkod[3] == "50" && t$zinkod[4] == "91" && t$zinkod[6] == "92" && 
              all(diff(t$NDZ_sanemsanas_datums[4:5]) == 0) && all(diff(t$NDZ_sanemsanas_datums[5:6]) != 0) &&
              all(diff(t$NDZ_sanemsanas_datums[1:4]) != 0)) {
    days1 <- as.numeric(difftime(t$beidz[1], prev, units = "days")) - 1 
    days2 <- as.numeric(difftime(t$beidz[3], t$sak[2], units = "days"))
    days3 <- as.numeric(difftime(t$last_date[6], t$sak[6], units = "days")) + 1 
    
    yt <- y2[v, ]
    yt$dienas <- sum(days1, days2, days3)
    rm(days1, days2, days3)
  } else if (all(t$zinkod[c(3,6)] == "50") && all(t$zinkod[c(2,4)] == "51") && t$zinkod[5] == "25" && 
             all(diff(t$NDZ_sanemsanas_datums[1:5]) != 0) && all(diff(t$NDZ_sanemsanas_datums[5:6]) == 0)) {
    days1 <- as.numeric(difftime(t$beidz[1], prev, units = "days")) - 1 
    days2 <- as.numeric(difftime(t$beidz[3], t$sak[2], units = "days"))
    days3 <- as.numeric(difftime(t$beidz[5], t$sak[4], units = "days"))
    
    yt <- y2[v, ]
    yt$dienas <- sum(days1, days2, days3)
    rm(days1, days2, days3)
  } else if (all(t$zinkod[c(3,5)] == "50") && all(t$zinkod[c(2,4)] == "51") && t$zinkod[6] == "25" && all(diff(t$NDZ_sanemsanas_datums) != 0)) {
    days1 <- as.numeric(difftime(t$beidz[1], prev, units = "days")) - 1 
    days2 <- as.numeric(difftime(t$beidz[3], t$sak[2], units = "days"))
    days3 <- as.numeric(difftime(t$beidz[5], t$sak[4], units = "days"))
    
    yt <- y2[v, ]
    yt$dienas <- sum(days1, days2, days3)
    rm(days1, days2, days3)
  } else if (all(t$zinkod[c(1,3)] == "50") && all(t$zinkod[c(2,4,6)] == "51") && t$zinkod[5] == "21" && 
             all(diff(t$NDZ_sanemsanas_datums[1:5]) != 0) && diff(t$NDZ_sanemsanas_datums[5:6]) == 0) {
    days1 <- as.numeric(difftime(t$beidz[1], prev, units = "days")) - 1 
    days2 <- as.numeric(difftime(t$beidz[3], t$sak[2], units = "days"))
    
    yt <- y2[v, ]
    yt$dienas <- sum(days1, days2)
    rm(days1, days2)
  } else {
    stop("starpkodi6_50: Trūkst izstrādes koda starpkodi.")
  }
  
  if(is.na(yt$PS_code[1])) {stop("Dienas NA.")}
  return(yt)
}

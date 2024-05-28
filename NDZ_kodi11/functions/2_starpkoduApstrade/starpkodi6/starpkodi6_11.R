starpkodi6_11 <- function(y2, t, prev, v) {
  
if (t$zinkod[2] == "53") {
  yt <- starpkodi6_11_53(y2, t, prev, v)  
} else if (t$zinkod[2] == "26" && 
             t$zinkod[3] == "11" && 
             t$zinkod[4] == "50" && 
             t$zinkod[5] == "51" && 
             t$zinkod[6] == "25" && 
             all(diff(t$NDZ_sanemsanas_datums[1:2]) == 0) &&
             all(diff(t$NDZ_sanemsanas_datums[2:6]) != 0)) {
    days1 <- as.numeric(difftime(t$beidz[4], t$sak[3], units = "days"))
    days2 <- as.numeric(difftime(t$beidz[6], t$sak[5], units = "days")) + 1 
    
    yt <- y2[v, ]
    yt$dienas <- sum(days1, days2)
    rm(days1, days2)
  } else if (t$zinkod[2] == "50" && 
             t$zinkod[3] == "51" && 
             t$zinkod[4] == "50" && 
             t$zinkod[5] == "25" && 
             t$zinkod[6] == "51" && 
             all(diff(t$NDZ_sanemsanas_datums[5:6]) == 0) &&
             all(diff(t$NDZ_sanemsanas_datums[1:5]) != 0)) {
    days1 <- as.numeric(difftime(t$beidz[2], t$sak[1], units = "days"))
    days2 <- as.numeric(difftime(t$beidz[4], t$sak[3], units = "days"))
    
    yt <- y2[v, ]
    yt$dienas <- sum(days1, days2)
    rm(days1, days2)
  } else if (t$zinkod[2] == "50" && 
             t$zinkod[3] == "51" && 
             t$zinkod[4] == "50" && 
             t$zinkod[5] == "51" && 
             t$zinkod[6] == "25" && 
             all(diff(t$NDZ_sanemsanas_datums) != 0)) {
    days1 <- as.numeric(difftime(t$beidz[2], t$sak[1], units = "days"))
    days2 <- as.numeric(difftime(t$beidz[4], t$sak[3], units = "days"))
    days3 <- as.numeric(difftime(t$beidz[6], t$sak[5], units = "days")) + 1 # jo atlaišana
    
    yt <- y2[v, ]
    yt$dienas <- sum(days1, days2, days3)
    rm(days1, days2, days3)
  } else if (t$zinkod[2] == "50" && t$zinkod[3] == "50" && 
             t$zinkod[4] == "51" && t$zinkod[5] == "21" && 
             t$zinkod[6] == "51" && all(sapply(seq(3, 6, by = 2), function(i) all(diff(t$NDZ_sanemsanas_datums[i+1]) == 0))) &&
             all(diff(t$NDZ_sanemsanas_datums[1:3]) != 0)) {
    yt <- y2[v, ]
    yt$dienas <- as.numeric(difftime(t$beidz[2], t$sak[1], units = "days"))
  } else if (all(t$zinkod[c(2,4)] == "50") && all(t$zinkod[c(3,6)] == "51") && t$zinkod[5] == "25" && 
             all(diff(t$NDZ_sanemsanas_datums) != 0)) {
    days1 <- as.numeric(difftime(t$beidz[2], t$sak[1], units = "days"))
    days2 <- as.numeric(difftime(t$beidz[4], t$sak[3], units = "days"))
    
    yt <- y2[v, ]
    yt$dienas <- sum(days1, days2)
    rm(days1, days2)
  } else if (all(t$zinkod[c(2,4)] == "50") && all(t$zinkod[c(3,6)] == "51") && t$zinkod[5] == "25" && 
             all(sapply(c(1,3,5), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) && 
             all(sapply(c(2,4), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
    days1 <- as.numeric(difftime(t$beidz[2], t$sak[1], units = "days"))
    days2 <- as.numeric(difftime(t$beidz[4], t$sak[3], units = "days"))
    
    yt <- y2[v, ]
    yt$dienas <- sum(days1, days2)
    rm(days1, days2)
  } else if (all(t$zinkod[seq(1,6,by=2)] == "11") && all(t$zinkod[c(2,4)] == "25") && t$zinkod[6] == "50" && 
       all(diff(t$NDZ_sanemsanas_datums) != 0)) {
    yt <- y2[v, ]
    yt$dienas <- sum(sapply(seq(1,6,by=2), function(i) as.numeric(difftime(t$beidz[i+1], t$sak[i], units = "days")) + 1)) - 1 
  } else if (all(t$zinkod[seq(2,6,by=2)] == "50") && all(t$zinkod[c(3,5)] == "51") && all(diff(t$NDZ_sanemsanas_datums[2:6]) != 0)) {
    yt <- y2[v, ]
    yt$dienas <- sum(sapply(seq(1,6,by=2), function(i) as.numeric(difftime(t$beidz[i+1], t$sak[i], units = "days"))))
  } else {
    stop("starpkodi6_11: Trūkst izstrādes koda starpkodi2.")
  }
  
  if(is.na(yt$PS_code[1])) {stop("Dienas NA.")}
  return(yt)
}

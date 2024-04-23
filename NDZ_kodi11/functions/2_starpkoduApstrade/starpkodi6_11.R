starpkodi6_11 <- function(y2, t, prev, v) {
  
   if (t$zinkod[2] %in% c("50", "53") &&
             t$zinkod[3] %in% c("51", "54") && 
             t$zinkod[4] %in% c("50", "53") && 
             t$zinkod[5] %in% c("51", "54") && 
             t$zinkod[6] %in% c("50", "21") && 
             all(!diff(t$NDZ_sanemsanas_datums) == 0)) {
    days1 <- as.numeric(difftime(t$beidz[2], t$sak[1], units = "days"))
    days2 <- as.numeric(difftime(t$beidz[4], t$sak[3], units = "days"))
    days3 <- as.numeric(difftime(t$beidz[6], t$sak[5], units = "days"))
    
    days <- sum(days1, days2, days3)
    rm(days1, days2, days3)
    
    yt <- y2[v, ]
    yt$dienas <- days
  } else if (t$zinkod[2] == "53" && 
             t$zinkod[3] == "54" && 
             t$zinkod[4] == "53" && 
             t$zinkod[5] == "53" && 
             t$zinkod[6] == "54" && 
             all(diff(t$NDZ_sanemsanas_datums[1:2]) == 0) &&
             all(diff(t$NDZ_sanemsanas_datums[5:6]) == 0) &&
             all(!diff(t$NDZ_sanemsanas_datums[2:5]) == 0)) {
    days1 <- as.numeric(difftime(t$beidz[2], t$sak[1], units = "days")) + 1 
    days2 <- as.numeric(difftime(t$beidz[4], t$sak[3], units = "days"))
    days3 <- as.numeric(difftime(t$beidz[5], t$sak[6], units = "days"))
    days <- sum(days1, days2, days3)
    rm(days1, days2, days3)
    
    yt <- y2[v, ]
    yt$dienas <- days
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
    days3 <- as.numeric(difftime(t$beidz[6], t$sak[5], units = "days")) + 1
    
    yt <- y2[v, ]
    yt$dienas <- sum(days1, days2, days3)
    rm(days1, days2, days3)
  } else if (t$zinkod[2] == "50" && t$zinkod[3] == "50" && 
             t$zinkod[4] == "51" && t$zinkod[5] == "21" && 
             t$zinkod[6] == "51" && all(sapply(seq(3, 6, by = 2), function(i) all(diff(t$NDZ_sanemsanas_datums[i+1]) == 0))) &&
             all(diff(t$NDZ_sanemsanas_datums[1:3]) != 0)) {
    yt <- y2[v, ]
    yt$dienas <- as.numeric(difftime(t$beidz[2], t$sak[1], units = "days"))
  } else {
    stop("starpkodi6_11: Trūkst izstrādes koda starpkodi2.")
  }
  
  if(is.na(yt$PS_code[1])) {stop("Dienas NA.")}
  return(yt)
}

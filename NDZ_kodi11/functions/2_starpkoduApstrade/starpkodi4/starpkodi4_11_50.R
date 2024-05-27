starpkodi4_11_50 <- function(y2, t, prev, v) {
  
  if(t$zinkod[3] == "51") {
    yt <- starpkodi4_11_50_51(y2, t, prev, v)
  } else if (t$zinkod[3] == "50" && t$zinkod[4] == "51" && 
             all(sapply(c(1,3), function(i) all(diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) &&
             all(diff(t$NDZ_sanemsanas_datums[2:3]) == 0)) {
    days1 <- as.numeric(difftime(t$beidz[2], t$sak[1], units = "days"))
    days2 <- as.numeric(difftime(t$last_date[4], t$sak[4], units = "days")) + 1
    
    yt <- y2[v, ]
    yt$dienas <- sum(days1, days2)
    rm(days1, days2)
  } else if (t$zinkod[3] == "25" && t$zinkod[4] == "51" && t$NDZ_sanemsanas_datums[1] != t$NDZ_sanemsanas_datums[2] && t$NDZ_sanemsanas_datums[2] != t$NDZ_sanemsanas_datums[3] &&
             t$NDZ_sanemsanas_datums[3] == t$NDZ_sanemsanas_datums[4]) {
    yt <- y2[v, ]
    yt$dienas <- as.numeric(difftime(as.Date(t$beidz[2]), as.Date(t$sak[1]), units = "days")) 
  } else if (t$zinkod[3] == "21" && t$zinkod[4] == "51" && all(!diff(t$NDZ_sanemsanas_datums[1:3]) == 0) &&
             t$NDZ_sanemsanas_datums[3] == t$NDZ_sanemsanas_datums[4]) {
    yt <- y2[v, ]
    yt$dienas <- as.numeric(difftime(as.Date(t$beidz[2]), as.Date(t$sak[1]), units = "days"))
  } else if (t$zinkod[3] == "22" && t$zinkod[4] == "51" && 
             all(diff(t$NDZ_sanemsanas_datums[1:3]) != 0) && t$NDZ_sanemsanas_datums[3] == t$NDZ_sanemsanas_datums[4]) {
    yt <- y2[v, ]
    yt$dienas <- as.numeric(difftime(t$beidz[2], t$sak[1], units = "days"))
  } else if (t$zinkod[3] == "50" && t$zinkod[4] == "51" && 
             all(sapply(c(1,3), function(i) all(diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) &&
             all(diff(t$NDZ_sanemsanas_datums[2:3]) == 0)) {
    days1 <- as.numeric(difftime(t$beidz[2], t$sak[1], units = "days"))
    days2 <- as.numeric(difftime(t$last_date[4], t$sak[4], units = "days")) + 1
    
    yt <- y2[v, ]
    yt$dienas <- sum(days1, days2)
    rm(days1, days2)
  } else if (t$zinkod[3] == "21" && t$zinkod[4] == "51" && all(diff(t$NDZ_sanemsanas_datums) != 0)) {
    yt <- y2[v, ]
    yt$dienas <- as.numeric(difftime(t$beidz[2], t$sak[1], units = "days"))
  } else if (t$zinkod[3] == "11" && t$zinkod[4] == "51" && 
             all(sapply(seq(1,4,by=2), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0))&&
             diff(t$NDZ_sanemsanas_datums[2:3]) != 0) {
    yt <- y2[v, ]
    yt$dienas <- as.numeric(difftime(t$last_date[2], t$sak[4], units = "days")) + 1
  } else if (t$zinkod[3] == "25" && t$zinkod[4] == "51" && 
             all(sapply(seq(1,4,by=2), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0))&&
             diff(t$NDZ_sanemsanas_datums[2:3]) != 0) {
    yt <- y2[v, ]
    yt$dienas <- 0
  } else {
    stop("Starpkodi4_11_50: Trūkst izstrādes koda.")
  }
  
  if(is.na(yt$PS_code[1])) {stop("Dienas NA.")}
  return(yt) 
}

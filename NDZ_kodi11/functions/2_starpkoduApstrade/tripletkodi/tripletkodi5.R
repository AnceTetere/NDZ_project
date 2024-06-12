tripletkodi5 <- function(y3, t, prev, v) {
  
  if (t$zinkod[1] == "91" && t$zinkod[2] == "51" && 
      t$zinkod[3] == "92" && t$zinkod[4] == "50" && t$zinkod[5] == "21" && 
      t$NDZ_sanemsanas_datums[1] == t$NDZ_sanemsanas_datums[2] &&
      t$NDZ_sanemsanas_datums[2] != t$NDZ_sanemsanas_datums[3] &&
      t$NDZ_sanemsanas_datums[3] == t$NDZ_sanemsanas_datums[4] &&
      t$NDZ_sanemsanas_datums[4] != t$NDZ_sanemsanas_datums[5]) {
    yt <- y3[v, ]
    yt$dienas <- 0
  } else if (t$zinkod[1] == "50" && t$zinkod[2] == "51" && 
       t$zinkod[3] == "53" && t$zinkod[4] == "54" && t$zinkod[5] == "21" && 
       t$NDZ_sanemsanas_datums[2] == t$NDZ_sanemsanas_datums[3] &&
       t$NDZ_sanemsanas_datums[1] != t$NDZ_sanemsanas_datums[2] &&
       all(diff(t$NDZ_sanemsanas_datums[3:5]) != 0)) {
    days1 <- as.numeric(difftime(t$beidz[1], prev, units = "days")) - 1 
    days2 <- as.numeric(difftime(t$beidz[5], t$sak[4], units = "days")) + 1 
    
    yt <- y3[v, ]
    yt$dienas <- sum(days1, days2)
    rm(days1, days2)
  } else if (t$zinkod[1] == "91" && t$zinkod[2] == "92" && 
             t$zinkod[3] == "50" && t$zinkod[4] == "25" && t$zinkod[5] == "51" && 
             all(sapply(seq(2, 5, by = 2), function(i) all(diff(t$NDZ_sanemsanas_datums[i:(i+1)])== 0))) &&
             t$NDZ_sanemsanas_datums[1] != t$NDZ_sanemsanas_datums[2]) {
    yt <- y3[v, ]
    yt$dienas <- as.numeric(difftime(t$beidz[1], prev, units = "days")) 
  } else if (t$zinkod[1] == "92" && t$zinkod[2] == "41" && t$zinkod[3] == "50" && t$zinkod[4] == "25" && t$zinkod[5] == "51" && 
             all(diff(t$NDZ_sanemsanas_datums[1:3])== 0) && all(diff(t$NDZ_sanemsanas_datums[3:5])!= 0)) {
    yt <- y3[v, ]
    yt$dienas <- 0
  } else {
    stop("Tripletkodi5 iztrūkst apstrādes koda.")
  }
  
  if(is.na(yt$PS_code[1])) {stop("Dienas NA.")}
  yt$zinkod <- "combined"  #jo tripletkodu dienu sarēķins
  return(yt)
}

starpkodi4_11 <- function(y2, t, prev, v) {
  if (t$zinkod[2] == "50" && t$NDZ_sanemsanas_datums[1] == t$NDZ_sanemsanas_datums[2] && 
      t$zinkod[3] == "51" && t$zinkod[4] == "21" && t$NDZ_sanemsanas_datums[3] != t$NDZ_sanemsanas_datums[4]) {

    yt <- y2[v, ]
    yt$dienas <- as.numeric(difftime(as.Date(t$beidz[4]), as.Date(t$sak[3]), units = "days"))
  } else if (t$zinkod[2] == "50" && t$NDZ_sanemsanas_datums[1] == t$NDZ_sanemsanas_datums[2] && 
             t$zinkod[3] == "51" && t$zinkod[4] == "25" && t$NDZ_sanemsanas_datums[3] != t$NDZ_sanemsanas_datums[4]) {
    yt <- y2[v, ]
    yt$dienas <- as.numeric(difftime(as.Date(t$beidz[4]), as.Date(t$sak[3]), units = "days"))
  } else if (t$zinkod[2] == "50" && t$zinkod[3] == "51" && t$zinkod[4] == "25" && all(diff(t$NDZ_sanemsanas_datums) != 0)) {
    days1 <- as.numeric(difftime(as.Date(t$beidz[2]), as.Date(t$sak[1]), units = "days")) 
    days2 <- as.numeric(difftime(as.Date(t$beidz[4]), as.Date(t$sak[3]), units = "days")) 
    
    yt <- y2[v, ]
    yt$dienas <- sum(days1, days2)
    rm(days1, days2)
  } else if (t$zinkod[2] == "91" && t$zinkod[3] == "92" && t$zinkod[4] == "91" && all(!diff(t$NDZ_sanemsanas_datums) == 0)) {
    days1 <- as.numeric(difftime(as.Date(t$beidz[2]), as.Date(t$sak[1]), units = "days")) 
    days2 <- as.numeric(difftime(as.Date(t$beidz[4]), as.Date(t$sak[3]), units = "days")) 
    
    yt <- y2[v, ]
    yt$dienas <- sum(days1, days2)
    rm(days1, days2)
  } else if (t$zinkod[2] == "50" && t$zinkod[3] == "51" && t$zinkod[4] == "50" && all(!diff(t$NDZ_sanemsanas_datums) == 0)) {
    days1 <- as.numeric(difftime(as.Date(t$beidz[2]), as.Date(t$sak[1]), units = "days")) 
    days2 <- as.numeric(difftime(as.Date(t$beidz[4]), as.Date(t$sak[3]), units = "days")) 

    yt <- y2[v, ]
    yt$dienas <-  sum(days1, days2)
    rm(days1, days2)
  } else if (t$zinkod[2] == "53" && t$zinkod[3] == "54" && t$zinkod[4] == "53" && all(!diff(t$NDZ_sanemsanas_datums) == 0)) {
    days1 <- as.numeric(difftime(as.Date(t$beidz[2]), as.Date(t$sak[1]), units = "days")) 
    days2 <- as.numeric(difftime(as.Date(t$beidz[4]), as.Date(t$sak[3]), units = "days"))
    
    yt <- y2[v, ]
    yt$dienas <- sum(days1, days2)
    rm(days1, days2)
  } else if (t$zinkod[2] == "50" && t$zinkod[3] == "25" && t$zinkod[4] == "51" && 
             t$NDZ_sanemsanas_datums[1] != t$NDZ_sanemsanas_datums[2] && t$NDZ_sanemsanas_datums[2] != t$NDZ_sanemsanas_datums[3] &&
             t$NDZ_sanemsanas_datums[3] == t$NDZ_sanemsanas_datums[4]) {
    yt <- y2[v, ]
    yt$dienas <- as.numeric(difftime(as.Date(t$beidz[2]), as.Date(t$sak[1]), units = "days")) 
  } else if (t$zinkod[2] == "50" && t$zinkod[3] == "21" && t$zinkod[4] == "51" && all(!diff(t$NDZ_sanemsanas_datums[1:3]) == 0) &&
             t$NDZ_sanemsanas_datums[3] == t$NDZ_sanemsanas_datums[4]) {
    yt <- y2[v, ]
    yt$dienas <- as.numeric(difftime(as.Date(t$beidz[2]), as.Date(t$sak[1]), units = "days"))
  } else if (t$zinkod[2] == "50" && t$zinkod[3] == "51" && t$zinkod[4] == "21" && all(!diff(t$NDZ_sanemsanas_datums) == 0)) {
    days1 <- as.numeric(difftime(t$beidz[2], t$sak[1], units = "days"))
    days2 <- as.numeric(difftime(t$beidz[4], t$sak[3], units = "days"))
    
    yt <- y2[v, ]
    yt$dienas <- sum(days1, days2)
    rm(days1, days2)
  } else if (t$zinkod[2] == "50" && t$zinkod[3] == "51" && t$zinkod[4] == "21" && 
             all(diff(t$NDZ_sanemsanas_datums[2:3]) == 0) && t$NDZ_sanemsanas_datums[1] != t$NDZ_sanemsanas_datums[2] &&
             t$NDZ_sanemsanas_datums[3] != t$NDZ_sanemsanas_datums[4]) {
    yt <- y2[v, ]
    yt$dienas <- as.numeric(difftime(t$beidz[4], t$sak[1], units = "days")) #parasti es te plusotu 1, jo darbs, bet izlaižu, jo tas bezalgas atvaļinājums jokainais vidū.
  } else if (t$zinkod[2] == "53" && t$zinkod[3] == "25" && t$zinkod[4] == "54" && 
             all(diff(t$NDZ_sanemsanas_datums[1:3]) != 0) && t$NDZ_sanemsanas_datums[3] == t$NDZ_sanemsanas_datums[4]) {

    yt <- y2[v, ]
    yt$dienas <- as.numeric(difftime(t$beidz[2], t$sak[1], units = "days"))
  } else {
    stop("Starpkodi4_11: Trūkst izstrādes koda.")
  }
  
  if(is.na(yt$PS_code[1])) {stop("Dienas NA.")}
  return(yt) 
}

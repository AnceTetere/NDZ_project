starpkodi3_11 <- function(y2, t, prev, v) {
  if (t$zinkod[2] == "50" && t$zinkod[3] == "51" && all(diff(t$NDZ_sanemsanas_datums) != 0)) {
    days1 <- as.numeric(difftime(as.Date(t$beidz[2]), as.Date(t$sak[1]), units = "days"))
    days2 <- as.numeric(difftime(as.Date(t$last_date[3]), as.Date(t$sak[3]), units = "days")) + 1 
    
    yt <- y2[v, ]
    yt$dienas <- sum(days1, days2)
    rm(days1, days2)
  } else if (t$zinkod[2] == "53" && t$zinkod[3] == "25" && all(diff(t$NDZ_sanemsanas_datums) != 0)) {
    yt <- y2[v, ]
    yt$dienas <- as.numeric(difftime(as.Date(t$beidz[2]), as.Date(t$sak[1]), units = "days"))
  } else if (t$zinkod[2] == "50" && t$zinkod[3] == "51" && all(diff(t$NDZ_sanemsanas_datums) != 0)) {
    yt <- y2[v, ]
    yt$dienas <- as.numeric(difftime(as.Date(t$last_date[3]), as.Date(t$sak[3]), units = "days")) + 1 
  } else if (t$zinkod[2] == "50" &&
             t$zinkod[3] == "51" && t$NDZ_sanemsanas_datums[1] == t$NDZ_sanemsanas_datums[2]) {
    yt <- y2[v, ]
    yt$dienas <- as.numeric(difftime(as.Date(t$last_date[3]), as.Date(t$sak[3]), units = "days")) + 1 
  } else if (t$zinkod[2] == "50" && t$zinkod[3] == "25" && all(!diff(t$NDZ_sanemsanas_datums) == 0)) {
    yt <- y2[v, ]
    yt$dienas <- as.numeric(difftime(as.Date(t$beidz[2]), as.Date(t$sak[1]), units = "days"))
  } else if (t$zinkod[2] == "53" && t$zinkod[3] == "21" && all(!diff(t$NDZ_sanemsanas_datums) == 0)) {
    yt <- y2[v, ]
    yt$dienas <- as.numeric(difftime(as.Date(t$beidz[2]), as.Date(t$sak[1]), units = "days"))
  } else if (t$zinkod[2] == "53" && t$zinkod[3] == "25" && all(!diff(t$NDZ_sanemsanas_datums[2:3]) == 0) && t$NDZ_sanemsanas_datums[1] == t$NDZ_sanemsanas_datums[2]) {
    yt <- y2[v, ]
    yt$dienas <- 1
  } else if (t$zinkod[2] == "25" && t$zinkod[3] == "53" && all(diff(t$NDZ_sanemsanas_datums[2:3]) == 0) &&
             t$NDZ_sanemsanas_datums[1] != t$NDZ_sanemsanas_datums[2]) {
    yt <- y2[v, ]
    yt$dienas <- as.numeric(difftime(t$beidz[3], t$sak[1], units = "days"))
  } else if (t$zinkod[2] == "53" && t$zinkod[3] == "54" && all(!diff(t$NDZ_sanemsanas_datums) == 0)) {
    days1 <- as.numeric(difftime(t$beidz[2], t$sak[1], units = "days"))
    days2 <- as.numeric(difftime(t$last_date[3], t$sak[3], units = "days")) + 1 
    
    yt <- y2[v, ]
    yt$dienas <- sum(days1, days2)
    rm(days1, days2)
  } else if (t$zinkod[2] == "50" && t$zinkod[3] == "21" && all(!diff(t$NDZ_sanemsanas_datums) == 0)) {
    yt <- y2[v, ]
    yt$dienas <- as.numeric(difftime(t$beidz[2], t$sak[1], units = "days"))
  } else if (t$zinkod[2] == "50" && t$zinkod[3] == "21" && t$NDZ_sanemsanas_datums[1] == t$NDZ_sanemsanas_datums[2] &&
               t$NDZ_sanemsanas_datums[2] != t$NDZ_sanemsanas_datums[3]) {
    yt <- y2[v, ]
    yt$dienas <- 0
  } else if (t$zinkod[2] == "21" && t$zinkod[3] == "50" && all(!diff(t$NDZ_sanemsanas_datums[1:2]) == 0) &&
             t$NDZ_sanemsanas_datums[2] == t$NDZ_sanemsanas_datums[3]) {
    yt <- y2[v, ]
    yt$dienas <- as.numeric(difftime(t$beidz[2], t$sak[1], units = "days"))
  } else if (t$zinkod[2] == "25" && t$zinkod[3] == "50" &&  all(!diff(t$NDZ_sanemsanas_datums[1:2]) == 0) &&
               t$NDZ_sanemsanas_datums[2] == t$NDZ_sanemsanas_datums[3]) {
    yt <- y2[v, ]
    yt$dienas <- as.numeric(difftime(t$beidz[3], t$sak[1], units = "days"))
  } else if (t$zinkod[2] == "21" && t$zinkod[3] == "53" && all(!diff(t$NDZ_sanemsanas_datums[1:2]) == 0) &&
               t$NDZ_sanemsanas_datums[2] == t$NDZ_sanemsanas_datums[3]) {
    yt <- y2[v:(v+1), ]
    yt <- yt[yt$zinkod == "11", ]
  } else if (t$zinkod[2] == "91" && t$zinkod[3] == "92" && all(!diff(t$NDZ_sanemsanas_datums) == 0)) {
    days1 <- as.numeric(difftime(t$beidz[2], t$sak[1], units = "days"))
    days2 <- as.numeric(difftime(t$last_date[3], t$sak[3], units = "days")) + 1
    
    yt <- y2[v, ]
    yt$dienas <- sum(days1, days2)
    rm(days1, days2)
  } else if ((t$zinkod[2] == "91" && t$zinkod[3] == "92" && all(diff(t$NDZ_sanemsanas_datums[2:3]) != 0) && all(diff(t$NDZ_sanemsanas_datums[1:2]) == 0)) 
             || 
             (t$zinkod[2] == "40" && t$zinkod[3] == "41" && all(diff(t$NDZ_sanemsanas_datums[2:3]) != 0) && all(diff(t$NDZ_sanemsanas_datums[1:2]) == 0))) {
    yt <- y2[v, ]
    yt$dienas <- as.numeric(difftime(t$last_date[3], t$sak[3], units = "days")) + 1 
  } else if (t$zinkod[2] == "50" && t$zinkod[3] == "50" && all(diff(t$NDZ_sanemsanas_datums) != 0)) {
    yt <- y2[v, ]
    yt$dienas <- as.numeric(difftime(t$beidz[3], t$sak[1], units = "days"))
  } else {
    stop("Starpkodi3_11: Trūkst izstrādes koda.")
  }
  
  if(is.na(yt$PS_code[1])) {stop("Dienas NA.")}
  return(yt) 
}


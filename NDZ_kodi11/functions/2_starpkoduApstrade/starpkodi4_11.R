starpkodi4_11 <- function(y2, t, prev, v) {
  
  if (t$zinkod[2] == "50") {
    yt <- starpkodi4_11_50(y2, t, prev, v)
  } else if (t$zinkod[2] == "91" && t$zinkod[3] == "92" && t$zinkod[4] == "91" && all(!diff(t$NDZ_sanemsanas_datums) == 0)) {
    days1 <- as.numeric(difftime(as.Date(t$beidz[2]), as.Date(t$sak[1]), units = "days")) 
    days2 <- as.numeric(difftime(as.Date(t$beidz[4]), as.Date(t$sak[3]), units = "days")) 
    
    yt <- y2[v, ]
    yt$dienas <- sum(days1, days2)
    rm(days1, days2)
  } else if (t$zinkod[2] == "53" && t$zinkod[3] == "54" && t$zinkod[4] == "53" && all(!diff(t$NDZ_sanemsanas_datums) == 0)) {
    days1 <- as.numeric(difftime(as.Date(t$beidz[2]), as.Date(t$sak[1]), units = "days")) 
    days2 <- as.numeric(difftime(as.Date(t$beidz[4]), as.Date(t$sak[3]), units = "days"))
    
    yt <- y2[v, ]
    yt$dienas <- sum(days1, days2)
    rm(days1, days2)
  } else if (t$zinkod[2] == "53" && t$zinkod[3] == "25" && t$zinkod[4] == "54" && 
             all(diff(t$NDZ_sanemsanas_datums[1:3]) != 0) && t$NDZ_sanemsanas_datums[3] == t$NDZ_sanemsanas_datums[4]) {
    yt <- y2[v, ]
    yt$dienas <- as.numeric(difftime(t$beidz[2], t$sak[1], units = "days"))
  } else if (t$zinkod[2] == "26" && t$zinkod[3] == "11" && t$zinkod[4] == "50" && 
             all(diff(t$NDZ_sanemsanas_datums[2:3]) != 0) && t$NDZ_sanemsanas_datums[1] == t$NDZ_sanemsanas_datums[2]) {
    yt <- y2[v, ]
    yt$dienas <- as.numeric(difftime(t$beidz[4], t$sak[3], units = "days"))
  } else if (t$zinkod[2] == "40" && t$zinkod[3] == "41" && t$zinkod[4] == "25" && 
             all(diff(t$NDZ_sanemsanas_datums[2:4]) != 0) && t$NDZ_sanemsanas_datums[1] == t$NDZ_sanemsanas_datums[2]) {
    yt <- y2[v, ] 
    yt$dienas <- as.numeric(difftime(t$beidz[4], t$sak[3], units = "days")) + 1 
  } else if (t$zinkod[2] == "91" && t$zinkod[3] == "25" && t$zinkod[4] == "92" && 
             all(sapply(seq(1,4, by = 2), function(i) all(diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0))) && 
             t$NDZ_sanemsanas_datums[2] != t$NDZ_sanemsanas_datums[3]) {
    yt <- y2[v, ]
    yt$dienas <- 0
  } else if (t$zinkod[2] == "25" && t$zinkod[3] == "11" && t$zinkod[4] == "50" && all(diff(t$NDZ_sanemsanas_datums) != 0)) {
    days1 <- as.numeric(difftime(t$beidz[2], t$sak[1], units = "days")) + 1 
    days2 <- as.numeric(difftime(t$beidz[4], t$sak[3], units = "days"))
    
    yt <- y2[v, ]
    yt$dienas <- sum(days1, days2)
    rm(days1, days2)
  } else if (t$zinkod[2] == "11" && t$zinkod[3] == "50" && t$zinkod[4] == "51" && all(diff(t$NDZ_sanemsanas_datums) != 0)) {
    days1 <- as.numeric(difftime(t$beidz[3], t$sak[2], units = "days"))
    days2 <- as.numeric(difftime(t$last_date[4], t$sak[4], units = "days")) + 1
    
    yt <- y2[v, ]
    yt$dienas <- sum(days1, days2)
    rm(days1, days2)
  } else if (t$zinkod[2] == "25" && t$zinkod[3] == "11" && t$zinkod[4] == "50" && 
              all(sapply(seq(1,4,by=2), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
              diff(t$NDZ_sanemsanas_datums[2:3]) != 0) {
     days1 <- as.numeric(difftime(t$beidz[2], t$sak[1], units = "days"))
     days2 <- as.numeric(difftime(t$beidz[4], t$sak[3], units = "days"))
     
     yt <- y2[v, ]
     yt$dienas <- sum(days1, days2)
     rm(days1, days2)
   } else if (t$zinkod[2] == "11" && t$zinkod[3] == "40" && t$zinkod[4] == "11" && 
              all(diff(t$NDZ_sanemsanas_datums[2:4]) != 0) && diff(t$NDZ_sanemsanas_datums[1:2]) == 0 &&
              t$PS_code[1] == '_________' && t$NM_code[1] == '____________') {
     yt <- y2[v, ]
     yt$dienas <- as.numeric(difftime(t$beidz[3], t$sak[2], units = "days"))
   } else {
    stop("Starpkodi4_11: Trūkst izstrādes koda.")
  }
  
  if(is.na(yt$PS_code[1])) {stop("Dienas NA.")}
  return(yt) 
}

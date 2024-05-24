starpkodi4_11 <- function(y2, t, prev, v) {
  
  if (t$zinkod[2] == "50") {
    yt <- starpkodi4_11_50(y2, t, prev, v)
  } else if (t$zinkod[2] == "91") {
    yt <- starpkodi4_11_91(y2, t, prev, v)
  } else if (t$zinkod[2] == "53") {
    yt <- starpkodi4_11_53(y2, t, prev, v)
  } else if (t$zinkod[2] == "26" && t$zinkod[3] == "11" && t$zinkod[4] == "50" && 
             all(diff(t$ZDN_sanemsanas_datums[2:3]) != 0) && t$ZDN_sanemsanas_datums[1] == t$ZDN_sanemsanas_datums[2]) {
    yt <- y2[v, ]
    yt$dienas <- as.numeric(difftime(t$beidz[4], t$sak[3], units = "days"))
  } else if (t$zinkod[2] == "40" && t$zinkod[3] == "41" && t$zinkod[4] == "25" && 
             all(diff(t$ZDN_sanemsanas_datums[2:4]) != 0) && t$ZDN_sanemsanas_datums[1] == t$ZDN_sanemsanas_datums[2]) {
    yt <- y2[v, ] 
    yt$dienas <- as.numeric(difftime(t$beidz[4], t$sak[3], units = "days")) + 1 
  } else if (t$zinkod[2] == "41" && t$zinkod[3] == "40" && t$zinkod[4] == "41" && 
             diff(t$ZDN_sanemsanas_datums[3:4]) != 0 && all(diff(t$ZDN_sanemsanas_datums[1:3]) == 0) &&
             t$PS_code[1] == '___________ && t$NM_code[1] == '_________') {
    yt <- y2[v, ] 
    yt$dienas <- as.numeric(difftime(t$last_date[4], t$sak[4], units = "days")) + 1 
  } else if (t$zinkod[2] == "25" && t$zinkod[3] == "11" && t$zinkod[4] == "50" && all(diff(t$ZDN_sanemsanas_datums) != 0)) {
    days1 <- as.numeric(difftime(t$beidz[2], t$sak[1], units = "days")) + 1 
    days2 <- as.numeric(difftime(t$beidz[4], t$sak[3], units = "days"))
    
    yt <- y2[v, ]
    yt$dienas <- sum(days1, days2)
    rm(days1, days2)
  } else if (t$zinkod[2] == "11" && t$zinkod[3] == "50" && t$zinkod[4] == "51" && all(diff(t$ZDN_sanemsanas_datums) != 0)) {
    days1 <- as.numeric(difftime(t$beidz[3], t$sak[2], units = "days"))
    days2 <- as.numeric(difftime(t$last_date[4], t$sak[4], units = "days")) + 1
    
    yt <- y2[v, ]
    yt$dienas <- sum(days1, days2)
    rm(days1, days2)
  } else if (t$zinkod[2] == "25" && t$zinkod[3] == "11" && t$zinkod[4] == "50" && 
              all(sapply(seq(1,4,by=2), function(i) diff(t$ZDN_sanemsanas_datums[i:(i+1)]) == 0)) &&
              diff(t$ZDN_sanemsanas_datums[2:3]) != 0) {
     days1 <- as.numeric(difftime(t$beidz[2], t$sak[1], units = "days"))
     days2 <- as.numeric(difftime(t$beidz[4], t$sak[3], units = "days"))
     
     yt <- y2[v, ]
     yt$dienas <- sum(days1, days2)
     rm(days1, days2)
   } else if (t$zinkod[2] == "11" && t$zinkod[3] == "40" && t$zinkod[4] == "11" && 
              all(diff(t$ZDN_sanemsanas_datums[2:4]) != 0) && diff(t$ZDN_sanemsanas_datums[1:2]) == 0 &&
              t$PS_code[1] == '__________' && t$NM_code[1] == '___________') {
     yt <- y2[v, ]
     yt$dienas <- as.numeric(difftime(t$beidz[3], t$sak[2], units = "days"))
   } else if (t$zinkod[2] == "11" && t$zinkod[3] == "50" && t$zinkod[4] == "51" && 
              all(diff(t$ZDN_sanemsanas_datums[2:4]) != 0) && diff(t$ZDN_sanemsanas_datums[1:2]) == 0) {
     days1 <- as.numeric(difftime(t$beidz[3], t$sak[2], units = "days"))
     days2 <- as.numeric(difftime(t$last_date[4], t$sak[4], units = "days")) + 1
     
     yt <- y2[v, ]
     yt$dienas <- sum(days1, days2)
     rm(days1,days2)
   } else if (t$zinkod[2] == "11" && t$zinkod[3] == "50" && t$zinkod[4] == "51" && 
              all(diff(t$ZDN_sanemsanas_datums[1:3]) == 0) && diff(t$ZDN_sanemsanas_datums[3:4]) != 0) {
     days1 <- as.numeric(difftime(t$beidz[3], t$sak[2], units = "days"))
     days2 <- as.numeric(difftime(t$last_date[4], t$sak[4], units = "days")) + 1
     
     yt <- y2[v, ]
     yt$dienas <- sum(days1, days2)
     rm(days1,days2)
   } else if (t$zinkod[2] == "92" && t$zinkod[3] == "40" && t$zinkod[4] == "41" && 
              all(diff(t$ZDN_sanemsanas_datums) == 0) && t$PS_code[1] == '_______' && t$NM_code[1] == '_______') {
     yt <- y2[v, ]
     yt$dienas <- 0
   } else {
    stop("Starpkodi4_11: Trūkst izstrādes koda.")
  }
  
  if(is.na(yt$PS_code[1])) {stop("Dienas NA.")}
  return(yt) 
}


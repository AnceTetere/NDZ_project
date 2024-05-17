starpkodi3_41 <- function(y2, t, prev, v) {
  
  if (t$zinkod[1] == "41" && t$zinkod[2] == "50" &&
           t$zinkod[3] == "51" && all(!diff(t$NDZ_sanemsanas_datums[2:3]) == 0) &&
           t$NDZ_sanemsanas_datums[1] == t$NDZ_sanemsanas_datums[2]) {
    yt <- y2[v, ]
    yt$dienas <- as.numeric(difftime(t$last_date[3], t$sak[3], units = "days")) + 1 
  } else if (t$zinkod[1] == "41" && t$zinkod[2] == "50" && t$zinkod[3] == "51" && 
             all(diff(t$NDZ_sanemsanas_datums) != 0)) {
    days1 <- as.numeric(difftime(t$beidz[2], t$sak[1], units = "days")) 
    days2 <- as.numeric(difftime(t$last_date[3], t$sak[3], units = "days"))  + 1 
    
    yt <- y2[v, ]
    yt$dienas <- sum(days1, days2)
    rm(days1, days2)
  } else if (t$zinkod[1] == "41" && t$zinkod[2] == "92" && t$zinkod[3] == "25" && 
             all(diff(t$NDZ_sanemsanas_datums[2:3]) != 0) && all(diff(t$NDZ_sanemsanas_datums[1:2]) == 0)) {
    yt <- y2[v, ]
    yt$dienas <- as.numeric(difftime(t$beidz[3], t$sak[1], units = "days"))
  } else if (t$zinkod[1] == "41" && t$zinkod[2] == "51" && t$zinkod[3] == "50" && all(diff(t$NDZ_sanemsanas_datums) == 0) &&
             t$PS_code[1] == '_________' && t$nmrkod[1] == '__________') {
    yt <- y2[v, ]
    yt$dienas <- as.numeric(difftime(t$last_date[1], t$sak[1], units = "days")) + 1 
  } else if (t$zinkod[2] == "41" && t$zinkod[3] == "25" && all(diff(t$NDZ_sanemsanas_datums) != 0)) {
    yt <- y2[v, ]
    yt$dienas <- as.numeric(difftime(t$beidz[3], t$sak[2], units = "days"))
  } else if (t$zinkod[2] == "53" && t$zinkod[3] == "54" && diff(t$NDZ_sanemsanas_datums[2:3]) != 0 && 
             diff(t$NDZ_sanemsanas_datums[1:2]) == 0) {
    yt <- y2[v, ]
    yt$dienas <- as.numeric(difftime(t$last_date[3], t$sak[3], units = "days")) + 1
  } else {
    stop("Starpkodi3_41: Trūkst izstrādes koda.")
  }
  
  if(is.na(yt$PS_code[1])) {stop("Dienas NA.")}
  return(yt) 
}

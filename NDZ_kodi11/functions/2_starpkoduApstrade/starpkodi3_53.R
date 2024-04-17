starpkodi3_53 <- function(y2, t, prev, v) {
  if (t$zinkod[1] == "53" && t$zinkod[2] == "54" &&
                t$zinkod[3] == "21" && all(diff(t$NDZ_sanemsanas_datums) != 0)) {
   
    days1 <- as.numeric(difftime(as.Date(t$beidz[1]), prev, units = "days")) - 1 
    days2 <- as.numeric(difftime(as.Date(t$beidz[3]), as.Date(t$sak[2]), units = "days")) 
    days <- days1 + days2
    rm(days1, days2)
    
    yt <- y2[v, ]
    yt$dienas <- days
  } else if (t$zinkod[1] == "53" && t$zinkod[2] == "54" &&
               t$zinkod[3] == "50" && all(!diff(t$NDZ_sanemsanas_datums) == 0)) {
 
    days1 <- as.numeric(difftime(t$beidz[1], prev, units = "days")) - 1 
    days2 <- as.numeric(difftime(t$beidz[3], t$sak[2], units = "days"))
    days <- days1 + days2
    rm(days1, days2)
    
    yt <- y2[v, ]
    yt$dienas <- days
  } else if (t$zinkod[1] == "53" && t$zinkod[2] == "54" &&
             t$zinkod[3] == "25" && 
             all(!diff(t$NDZ_sanemsanas_datums) == 0)) {

    days1 <- as.numeric(difftime(t$beidz[1], prev, units = "days")) - 1 
    days2 <- as.numeric(difftime(t$beidz[3], t$sak[2], units = "days")) + 1 
    days <- sum(days1, days2)
    rm(days1, days2)
    
    yt <- y2[v, ]
    yt$dienas <- days
  } else if (t$zinkod[1] == "53" && t$zinkod[2] == "25" &&
             t$zinkod[3] == "54" && 
             all(!diff(t$NDZ_sanemsanas_datums[1:2]) == 0) &&
             t$NDZ_sanemsanas_datums[2] == t$NDZ_sanemsanas_datums[3]) {

    yt <- y2[v, ]
    yt$dienas <- as.numeric(difftime(t$beidz[1], prev, units = "days"))
  } else {
    stop("Starpkodi3_53: Trūkst izstrādes koda.")
  }
  
  if(is.na(yt$PS_code[1])) {stop("Dienas NA.")}
  return(yt) 
}

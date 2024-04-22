starpkodi5 <- function(y2, t, prev, v) {

  if (t$zinkod[1] == "50") {
    yt <- starpkodi5_50(y2, t, prev, v)
  } else if (t$zinkod[1] == "91") {
    yt <- starpkodi5_91(y2, t, prev, v)
  } else if (t$zinkod[1] == "11") {
    yt <- starpkodi5_11(y2, t, prev, v)
  } else if(t$zinkod[1] == "25" && t$zinkod[2] == "11" && 
            t$NDZ_sanemsanas_datums[1] != t$NDZ_sanemsanas_datums[2] && 
            t$NDZ_sanemsanas_datums[2] == t$NDZ_sanemsanas_datums[3] && 
            t$zinkod[3] == "81" && t$zinkod[4] == "25" && 
            t$NDZ_sanemsanas_datums[3] != t$NDZ_sanemsanas_datums[4] && 
            t$zinkod[5] == "82" && 
            t$NDZ_sanemsanas_datums[4] == t$NDZ_sanemsanas_datums[5]) {

    days1 <- as.numeric(difftime(as.Date(t$beidz[1]), prev, units = "days"))
    days2 <- as.numeric(difftime(as.Date(t$beidz[5]), as.Date(t$sak[3]), units = "days")) + 1 # jo darbs
    days <- days1 + days2
    rm(days1, days2)
    
    yt <- y2[v, ]
    yt$dienas <- days
  } else if(t$zinkod[1] == "25" && t$zinkod[2] == "25" && 
            t$zinkod[3] == "11" && t$zinkod[4] == "81" && 
            t$zinkod[5] == "82" && t$NDZ_sanemsanas_datums[1] != t$NDZ_sanemsanas_datums[2] &&
            all(diff(t$NDZ_sanemsanas_datums[2:5]) == 0)) {

    days1 <- as.numeric(difftime(t$beidz[1], prev, units = "days")) 
    days2 <- 1 #uz vienu dienu paņemts
    days <- days1 + days2
    rm(days1, days2)
    
    yt <- y2[v, ]
    yt$dienas <- days
  } else if(t$zinkod[1] == "25" && t$zinkod[2] == "26" && 
            t$zinkod[3] == "11" && t$zinkod[4] == "11" && 
            t$zinkod[5] == "81" && 
            t$NDZ_sanemsanas_datums[1] != t$NDZ_sanemsanas_datums[2] &&
            t$NDZ_sanemsanas_datums[2] == t$NDZ_sanemsanas_datums[3] &&
            t$NDZ_sanemsanas_datums[4] == t$NDZ_sanemsanas_datums[5]) {

    days1 <- as.numeric(difftime(t$beidz[1], prev, units = "days")) 
    days2 <- 0 #darba neuzsākšana
    days3 <- as.numeric(difftime(t$last_date[5], t$sak[5], units = "days"))
    
    days <- days1 + days2 + days3
    rm(days1, days2, days3)
    
    yt <- y2[v, ]
    yt$dienas <- days
  } else if(t$zinkod[1] == "25" && t$zinkod[2] == "11" && 
           t$zinkod[3] == "81" && t$zinkod[4] == "82" && 
           t$zinkod[5] == "11" && 
           all(diff(t$NDZ_sanemsanas_datums[1:4]) == 0) &&
           t$NDZ_sanemsanas_datums[4] != t$NDZ_sanemsanas_datums[5]) {

    days1 <- as.numeric(difftime(t$beidz[4], t$sak[3], units = "days")) + 1 
    days2 <- as.numeric(difftime(t$last_date[5], t$sak[5], units = "days")) +1 
    days <- sum(days1, days2)
    rm(days1, days2)
    
    yt <- y2[v, ]
    yt$dienas <- days
  } else {
    stop("Starpkodi5: iztrūkst apstrādes koda.")
  }
  if(is.na(yt$PS_code[1])) {stop("Dienas NA.")}
  yt$zinkod <- "combined"  #jo starpkodu dienu sarēķins
  return(yt)
}

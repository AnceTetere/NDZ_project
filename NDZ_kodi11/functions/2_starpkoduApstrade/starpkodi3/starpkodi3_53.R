starpkodi3_53 <- function(y2, t, prev, v) {
  if (t$zinkod[2] == "54" && t$zinkod[3] == "21" && all(diff(t$NDZ_sanemsanas_datums) != 0)) {
    days1 <- as.numeric(difftime(as.Date(t$beidz[1]), prev, units = "days")) - 1 # jo dīkstāve, beidz datumā darbinieks au ir dīkstāvē
    days2 <- as.numeric(difftime(as.Date(t$beidz[3]), as.Date(t$sak[2]), units = "days")) 
    
    yt <- y2[v, ]
    yt$dienas <- sum(days1, days2)
    rm(days1, days2)
  } else if (t$zinkod[2] == "54" && t$zinkod[3] == "50" && all(!diff(t$NDZ_sanemsanas_datums) == 0)) {
    days1 <- as.numeric(difftime(t$beidz[1], prev, units = "days")) - 1 
    days2 <- as.numeric(difftime(t$beidz[3], t$sak[2], units = "days"))
    
    yt <- y2[v, ]
    yt$dienas <- sum(days1, days2)
    rm(days1, days2)
  } else if (t$zinkod[2] == "54" && t$zinkod[3] %in% c("24", "25") && all(!diff(t$NDZ_sanemsanas_datums) == 0)) {
    days1 <- as.numeric(difftime(t$beidz[1], prev, units = "days")) - 1 
    days2 <- as.numeric(difftime(t$beidz[3], t$sak[2], units = "days")) + 1 
    
    yt <- y2[v, ]
    yt$dienas <- sum(days1, days2)
    rm(days1, days2)
  } else if (t$zinkod[2] == "25" && t$zinkod[3] == "54" && 
             all(!diff(t$NDZ_sanemsanas_datums[1:2]) == 0) && t$NDZ_sanemsanas_datums[2] == t$NDZ_sanemsanas_datums[3]) {
    yt <- y2[v, ]
    yt$dienas <- as.numeric(difftime(t$beidz[1], prev, units = "days"))
  } else if (t$zinkod[2] == "21" && t$zinkod[3] == "54" && 
              all(diff(t$NDZ_sanemsanas_datums[1:2]) != 0) && t$NDZ_sanemsanas_datums[2] == t$NDZ_sanemsanas_datums[3]) {
    yt <- y2[v, ]
    yt$dienas <- as.numeric(difftime(t$beidz[1], prev, units = "days")) - 1 
  } else if (t$zinkod[2] == "54" && t$zinkod[3] == "21" && 
             all(diff(t$NDZ_sanemsanas_datums[1:2]) == 0) && t$NDZ_sanemsanas_datums[2] != t$NDZ_sanemsanas_datums[3]) {
    yt <- y2[v:(v+1), ]
    yt <- yt[yt$zinkod == "11", ]
  } else if (t$zinkod[2] == "22" && t$zinkod[3] == "54") {
    yt <- y2[v, ]
    yt$dienas <- as.numeric(difftime(t$beidz[1], prev, units = "days"))
  } else if (t$zinkod[2] == "40" && t$zinkod[3] == "54" && diff(t$NDZ_sanemsanas_datums[1:2]) != 0 && diff(t$NDZ_sanemsanas_datums[2:3]) == 0) {
    yt <- y2[v, ]
    yt$dienas <- as.numeric(difftime(t$beidz[1], prev, units = "days")) - 1
  } else {
    stop("Starpkodi3_53: Trūkst izstrādes koda.")
  }
  
  if(is.na(yt$PS_code[1])) {stop("Dienas NA.")}
  return(yt) 
}

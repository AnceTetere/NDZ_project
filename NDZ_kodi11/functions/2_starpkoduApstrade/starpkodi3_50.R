starpkodi3_50 <- function(y2, t, prev, v) {
    
  if(t$zinkod[2] == "51") {
    yt <- starpkodi3_50_51(y2, t, prev, v)
  } else if (t$zinkod[2] == "21" && t$zinkod[3] == "51" && 
             t$NDZ_sanemsanas_datums[2] == t$NDZ_sanemsanas_datums[3]){
    yt <- y2[v, ]
    yt$dienas <- as.numeric(difftime(t$beidz[1], prev, units = "days"))
  } else if (t$zinkod[2] == "25" && t$zinkod[3] == "51" && t$NDZ_sanemsanas_datums[2] == t$NDZ_sanemsanas_datums[3]) {
    days <- as.numeric(difftime(t$beidz[1], prev, units = "days")) - 1 #mīnus 1, jo 10-tajā viņš jau ir atvaļinājumā
    
    yt <- y2[v, ]
    yt$dienas <- days
  } else if (t$zinkod[2] == "91" && t$zinkod[3] == "51" && all(diff(t$NDZ_sanemsanas_datums[1:2]) != 0) && all(diff(t$NDZ_sanemsanas_datums[2:3]) == 0)) {
    yt <- y2[v, ]
    yt$dienas <- as.numeric(difftime(t$beidz[1], prev, units = "days")) - 1 # jo atvaļinājums
  } else if (t$zinkod[2] == "11" && t$zinkod[3] == "51" && all(diff(t$NDZ_sanemsanas_datums[1:2]) != 0) && all(diff(t$NDZ_sanemsanas_datums[2:3]) == 0)) {
    yt <- y2[v:(v+1), ]
    yt <- yt[yt$zinkod == "50", ]
  } else if (t$zinkod[2] == "50" && t$zinkod[3] == "25" && all(diff(t$NDZ_sanemsanas_datums) != 0)) {
    yt <- y2[v:(v+1), ]
    yt <- yt[yt$zinkod == "50", ]
  } else if (t$zinkod[2] == "21" && t$zinkod[3] == "51" && all(diff(t$NDZ_sanemsanas_datums) != 0)) {
    yt <- y2[v, ]
    yt$dienas <- as.numeric(difftime(t$beidz[1], prev, units = "days")) - 1 
  } else if (t$zinkod[2] == "50" && t$zinkod[3] == "21" && all(diff(t$NDZ_sanemsanas_datums) != 0)) {
    yt <- y2[v, ]
    yt$dienas <- as.numeric(difftime(t$beidz[2], prev, units = "days")) - 1 
  } else if (t$zinkod[2] == "50" && t$zinkod[3] == "21" && diff(t$NDZ_sanemsanas_datums[2:3]) != 0 && diff(t$NDZ_sanemsanas_datums[1:2]) == 0) {
    yt <- y2[v, ]
    yt$dienas <- as.numeric(difftime(t$beidz[2], prev, units = "days")) - 1 
  } else if (t$zinkod[2] == "22" && t$zinkod[3] == "51" && all(diff(t$NDZ_sanemsanas_datums[1:2]) != 0) && all(diff(t$NDZ_sanemsanas_datums[2:3]) == 0)) {
    yt <- y2[v, ]
    yt$dienas <- as.numeric(difftime(t$beidz[1], prev, units = "days")) - 1
  } else if (t$zinkod[2] == "25" && t$zinkod[3] == "51" && all(diff(t$NDZ_sanemsanas_datums) != 0)) {
    yt <- y2[v, ]
    yt$dienas <- as.numeric(difftime(t$beidz[1], prev, units = "days")) - 1
  } else if (t$zinkod[2] == "40" && t$zinkod[3] == "51" && diff(t$NDZ_sanemsanas_datums[1:2]) != 0 && diff(t$NDZ_sanemsanas_datums[2:3]) == 0) {
    yt <- y2[v, ]
    yt$dienas <- as.numeric(difftime(t$beidz[1], prev, units = "days")) - 1
  } else if (t$zinkod[2] == "53" && t$zinkod[3] == "54" && 
             diff(t$NDZ_sanemsanas_datums[1:2]) == 0 && diff(t$NDZ_sanemsanas_datums[2:3]) != 0 &&
             t$PS_code[1] == '________' && t$NM_code[1] == '________') {
    days1 <- as.numeric(difftime(t$beidz[2], prev, units = "days")) - 1 #jo dīkstāve
    days2 <- as.numeric(difftime(t$last_date[3], t$sak[3], units = "days")) + 1 
    
    yt <- y2[v, ]
    yt$dienas <- sum(days1, days2)
    rm(days1, days2)
  } else {
    stop("starpkodi3_50: Iztrūkst apstrādes koda.")
  }
  
  if(is.na(yt$PS_code[1])) {stop("Dienas NA.")}
  return(yt) 
}

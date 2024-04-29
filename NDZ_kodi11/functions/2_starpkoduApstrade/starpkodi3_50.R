starpkodi3_50 <- function(y2, t, prev, v) {

if (t$zinkod[2] == "21" && t$zinkod[3] == "51" && 
    t$NDZ_sanemsanas_datums[2] == t$NDZ_sanemsanas_datums[3]){
  yt <- y2[v, ]
  yt$dienas <- as.numeric(difftime(as.Date(t$beidz[1]), prev, units = "days"))
} else if (t$zinkod[2] == "51" && t$zinkod[3] == "25" && 
           all(!diff(t$NDZ_sanemsanas_datums) == 0)) {
  days1 <- as.numeric(difftime(as.Date(t$beidz[1]), prev, units = "days"))
  days2 <- as.numeric(difftime(as.Date(t$beidz[3]), as.Date(t$sak[2]), units = "days"))
  days <- days1 + days2
  
  yt <- y2[v, ]
  yt$dienas <- days
} else if (t$zinkod[2] == "25" && t$zinkod[3] == "51" && t$NDZ_sanemsanas_datums[2] == t$NDZ_sanemsanas_datums[3]) {
  days <- as.numeric(difftime(as.Date(t$beidz[1]), prev, units = "days")) - 1 
  yt <- y2[v, ]
  yt$dienas <- days
} else if (t$zinkod[2] == "51" && t$zinkod[3] == "21" && all(diff(t$NDZ_sanemsanas_datums) != 0)) {
  days1 <- as.numeric(difftime(as.Date(t$beidz[1]), prev, units = "days")) - 1 # jo atvaļinājums
  days2 <- as.numeric(difftime(as.Date(t$beidz[3]), as.Date(t$sak[2]), units = "days")) 
  days <- days1 + days2
  rm(days1, days2)
  
  yt <- y2[v, ]
  yt$dienas <- days
} else if (t$zinkod[2] == "51" && t$zinkod[3] == "91" && all(!diff(t$NDZ_sanemsanas_datums) == 0)) {

  days1 <- as.numeric(difftime(as.Date(t$beidz[1]), prev, units = "days")) -1 
  days2 <- as.numeric(difftime(as.Date(t$beidz[3]), as.Date(t$sak[2]), units = "days")) + 1 
  days <- days1 + days2
  rm(days1, days2)
  yt <- y2[v, ]
  yt$dienas <- days
} else if (t$zinkod[2] == "51" && t$zinkod[3] == "25" && all(diff(t$NDZ_sanemsanas_datums[1:2]) == 0) && all(diff(t$NDZ_sanemsanas_datums[2:3]) != 0)) {
  yt <- y2[v:(v+1), ]
  yt <- yt[yt$zinkod == "11", ]
} else if (t$zinkod[2] == "51" && t$zinkod[3] == "21" && all(diff(t$NDZ_sanemsanas_datums[1:2]) == 0) && all(diff(t$NDZ_sanemsanas_datums[2:3]) != 0)) {
  yt <- y2[v:(v+1), ]
  yt <- yt[yt$zinkod == "11", ]
} else if (t$zinkod[2] == "91" && t$zinkod[3] == "51" && all(diff(t$NDZ_sanemsanas_datums[1:2]) != 0) && all(diff(t$NDZ_sanemsanas_datums[2:3]) == 0)) {
  yt <- y2[v, ]
  yt$dienas <- as.numeric(difftime(t$beidz[1], prev, units = "days")) - 1 # jo atvaļinājums
} else if (t$zinkod[2] == "11" && t$zinkod[3] == "51" && all(diff(t$NDZ_sanemsanas_datums[1:2]) != 0) && all(diff(t$NDZ_sanemsanas_datums[2:3]) == 0)) {
  yt <- y2[v:(v+1), ]
  yt <- yt[yt$zinkod == "50", ]
} else {
  stop("starpkodi3_50: Iztrūkst apstrādes koda.")
}
  
  if(is.na(yt$PS_code[1])) {stop("Dienas NA.")}
  return(yt) 
}

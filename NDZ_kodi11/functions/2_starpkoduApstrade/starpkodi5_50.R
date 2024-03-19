starpkodi5_50 <- function(y2, t, prev) {
  
  if (t$zinkod[2] == "51" && t$NDZ_sanemsanas_datums[1] != t$NDZ_sanemsanas_datums[2] && t$zinkod[3] == "50" && t$zinkod[4] == "51" && t$NDZ_sanemsanas_datums[2] != t$NDZ_sanemsanas_datums[3] && t$zinkod[5] == "21" && t$NDZ_sanemsanas_datums[4] != t$NDZ_sanemsanas_datums[5]) {
  
  days1 <- as.numeric(difftime(as.Date(t$beidz[1]), prev, units = "days")) - 1 #jo atvaļinājums
  days2 <- as.numeric(difftime(as.Date(t$beidz[3]), as.Date(t$sak[2]), units = "days"))
  days3 <- as.numeric(difftime(as.Date(t$beidz[5]), as.Date(t$sak[4]), units = "days"))
  days <- days1 + days2 + days3
  rm(days1, days2, days3)
  
  yt <- y2[v, ]
  yt$dienas <- days
} else if (t$zinkod[2] == "51" && 
           t$NDZ_sanemsanas_datums[1] != t$NDZ_sanemsanas_datums[2] && 
           t$zinkod[3] == "50" && t$zinkod[4] == "25" && 
           t$NDZ_sanemsanas_datums[3] != t$NDZ_sanemsanas_datums[4] && 
           t$zinkod[5] == "51" && t$NDZ_sanemsanas_datums[4] == t$NDZ_sanemsanas_datums[5]) {
  days1 <- as.numeric(difftime(as.Date(t$beidz[1]), prev, units = "days")) - 1 #jo atvaļinājums apgriezti, Norādītajā datumā persona jau ir brīva.
  days2 <- as.numeric(difftime(as.Date(t$beidz[3]), as.Date(t$sak[2]), units = "days")) - 1
  days <- days1 + days2
  rm(days1, days2)
  
  yt <- y2[v, ]
  yt$dienas <- days
} else if(t$zinkod[2] == "51" && 
          t$zinkod[3] == "25" && t$zinkod[4] == "51" && 
          t$zinkod[5] == "11" && t$NDZ_sanemsanas_datums[1] == t$NDZ_sanemsanas_datums[2] &&
          t$NDZ_sanemsanas_datums[3] == t$NDZ_sanemsanas_datums[4] &&
          t$NDZ_sanemsanas_datums[4] != t$NDZ_sanemsanas_datums[5]) {
  days1 <- as.numeric(difftime(t$beidz[1], prev, units = "days")) - 1 #jo atvaļinājums 
  days2 <- as.numeric(difftime(t$last_date[5], t$sak[5], units = "days")) + 1 #jo darbs
  days <- days1 + days2
  rm(days1, days2)
  
  yt <- y2[v, ]
  yt$dienas <- days
} else if(t$zinkod[2] == "51" && 
          t$zinkod[3] == "50" && t$zinkod[4] == "51" && 
          t$zinkod[5] == "25" && 
          all(!diff(t$NDZ_sanemsanas_datums) == 0)) {
  days1 <- as.numeric(difftime(t$beidz[1], prev, units = "days")) - 1 #jo atvaļinājums
  days2 <- as.numeric(difftime(t$beidz[3], t$sak[2], units = "days"))
  days3 <- as.numeric(difftime(t$beidz[5], t$sak[4], units = "days"))
  
  days <- days1 + days2 + days3
  rm(days1, days2, days3)
  
  yt <- y2[v, ]
  yt$dienas <- days
} else if(t$zinkod[2] == "51" && 
          t$zinkod[3] == "91" && t$zinkod[4] == "92" && 
          t$zinkod[5] == "40" && 
          all(!diff(t$NDZ_sanemsanas_datums[1:4]) == 0) &&
          t$NDZ_sanemsanas_datums[4] == t$NDZ_sanemsanas_datums[5]) {
  days1 <- as.numeric(difftime(t$beidz[1], prev, units = "days")) - 1 #jo atvaļinājums
  days2 <- as.numeric(difftime(t$beidz[3], t$sak[2], units = "days"))
  days3 <- as.numeric(difftime(t$beidz[5], t$sak[4], units = "days"))
  
  days <- days1 + days2 + days3
  rm(days1, days2, days3)
  
  yt <- y2[v, ]
  yt$dienas <- days
}else {
  stop("Starpkodi5_50: iztrūkst apstrādes koda.")
}

return(yt)
}

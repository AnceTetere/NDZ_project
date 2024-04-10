starpkodi3_25 <- function(y2, t, prev) {
  if (t$zinkod[2] == "51" && t$zinkod[3] == "50" && all(t$NDZ_sanemsanas_datums == t$NDZ_sanemsanas_datums[1])) {

    yt <- y2[v:(v+1), ]
    yt <- yt[yt$zinkod == "11", ]
  #} else if (t$zinkod[2] == "11" && t$zinkod[3] == "81" && all(diff(t$NDZ_sanemsanas_datums) != 0)) {

  #  days1 <- as.numeric(difftime(as.Date(t$beidz[1]), prev, units = "days"))
  #  days2 <- as.numeric(difftime(as.Date(t$last_date[3]), as.Date(t$sak[2]), units = "days")) 
  #  days <- days1 + days2 
  #  rm(days1, days2)
    
  #  yt <- y2[v, ]
  #  yt$dienas <- days
  #} else if (t$zinkod[2] == "11" && t$zinkod[3] == "81" && t$NDZ_sanemsanas_datums[1] != t$NDZ_sanemsanas_datums[2] &&
  #           t$NDZ_sanemsanas_datums[2] == t$NDZ_sanemsanas_datums[3]) {

  #  days1 <- as.numeric(difftime(as.Date(t$beidz[1]), prev, units = "days"))
  #  days2 <- as.numeric(difftime(as.Date(t$last_date[3]), as.Date(t$sak[3]), units = "days")) + 1 #jo darbs 
  #  days <- days1 + days2
  #  rm(days1, days2)
    
  #  yt <- y2[v, ]
  #  yt$dienas <- days
  #} else if (t$zinkod[2] == "82" && t$zinkod[3] == "11" && all(!diff(t$NDZ_sanemsanas_datums[2:3]) == 0) &&
  #           t$NDZ_sanemsanas_datums[1] == t$NDZ_sanemsanas_datums[2]) {

  #  yt <- y2[v:(v+1), ]
  #  yt <- yt[yt$zinkod == "11", ]
  } else if (t$zinkod[2] == "51" && t$zinkod[3] == "11" && t$NDZ_sanemsanas_datums[1] == t$NDZ_sanemsanas_datums[2] &&
             t$NDZ_sanemsanas_datums[2] != t$NDZ_sanemsanas_datums[3]) {
    
    yt <- y2[v, ]
    yt$dienas <- as.numeric(difftime(as.Date(t$last_date[3]), as.Date(t$sak[3]), units = "days")) + 1 + 1 #jo darbs (+1) un viena nostrādātā diena, kad atlaida (+1)
  } else {
    stop("Starpkodi3_25: Trūkst izstrādes koda.")
  }

  if(is.na(yt$PS_code[1])) {stop("Dienas NA.")}
  return(yt) 
}

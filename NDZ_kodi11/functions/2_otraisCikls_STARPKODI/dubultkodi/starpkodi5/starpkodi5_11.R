starpkodi5_11 <- function(y, t, prev, v) {

  if (t$zinkod[2] == "50") {
     yt <- starpkodi5_11_50(y, t, prev, v)
  } else if (t$zinkod[2] == "25") {
    yt <- starpkodi5_11_25(y, t, prev, v)
  #} else if(t$zinkod[2] == "81" && t$zinkod[3] == "82" && t$zinkod[4] == "81" && 
  #          t$zinkod[5] == "82" && all(!diff(t$NDZ_sanemsanas_datums) == 0)) {
  #  days1 <- as.numeric(difftime(as.Date(t$NDZ_sanemsanas_datums[3]), as.Date(t$NDZ_sanemsanas_datums[2]), units = "days")) + 1 
  #  days2 <- as.numeric(difftime(as.Date(t$NDZ_sanemsanas_datums[5]), as.Date(t$NDZ_sanemsanas_datums[4]), units = "days")) + 1
  #  days <- days1 + days2
  #  rm(days1, days2)
  #  
  #  yt <- y[v, ]
  #  yt$dienas <- days
  #} else if(t$zinkod[2] == "91" && t$zinkod[3] == "92" && t$zinkod[4] == "91" && 
  #          t$zinkod[5] == "92" && all(!diff(t$NDZ_sanemsanas_datums) == 0)) {
  #  days1 <- as.numeric(difftime(as.Date(t$NDZ_sanemsanas_datums[2]), as.Date(t$NDZ_sanemsanas_datums[1]), units = "days"))
  #  days2 <- as.numeric(difftime(as.Date(t$NDZ_sanemsanas_datums[4]), as.Date(t$NDZ_sanemsanas_datums[3]), units = "days"))
  #  days3 <- as.numeric(difftime(as.Date(t$last_date[5]), as.Date(t$NDZ_sanemsanas_datums[5]), units = "days")) + 1 
  #  days <- days1 + days2 + days3
  #  rm(days1, days2, days3)
  #  
  #  yt <- y[v, ]
  #  yt$dienas <- days
  #} else if(t$zinkod[2] == "81" && t$zinkod[3] == "25" && t$zinkod[4] == "82" && 
  #          t$zinkod[5] == "11" && t$NDZ_sanemsanas_datums[1] == t$NDZ_sanemsanas_datums[2] &&
  #          t$NDZ_sanemsanas_datums[3] == t$NDZ_sanemsanas_datums[4]) {
  #  yt <- y[v:(v+1), ]
  #  yt <- yt[yt$zinkod == "11", ]
  #} else if(t$zinkod[2] == "26" && t$zinkod[3] == "11" && t$zinkod[4] == "50" && 
  #          t$zinkod[5] == "51" && 
  #          all(diff(t$NDZ_sanemsanas_datums[2:5]) != 0) &&
  #          all(diff(t$NDZ_sanemsanas_datums[1:2]) == 0)) {
  #  days1 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[4], t$NDZ_sanemsanas_datums[3], units = "days"))
  #  days2 <- as.numeric(difftime(t$last_date[5], t$NDZ_sanemsanas_datums[5], units = "days")) + 1 
  #  
  #  yt <- y[v, ]
  #  yt$dienas <- sum(days1, days2)
  #  rm(days1, days2)
  #} else if (t$zinkod[2] == "11" && t$zinkod[3] == "50" && t$zinkod[4] == "51" && t$zinkod[5] == "51" && 
  #           all(diff(t$NDZ_sanemsanas_datums) != 0)) {
  #  days1 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[3], t$NDZ_sanemsanas_datums[2], units = "days"))
  #  days2 <- as.numeric(difftime(t$last_date[5], t$NDZ_sanemsanas_datums[5], units = "days")) + 1
  #  
  #  yt <- y[v, ]
  #  yt$dienas <- sum(days1, days2)
  #  rm(days1, days2)
  } else {stop("Starpkodi5_11: Trūkst izstrādes koda.")}
  
  rm(y, t, prev, v)
  return(yt) 
}

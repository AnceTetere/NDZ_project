starpkodi5_11_50 <- function(y, t, prev, v) {
  
  if (t$zinkod[3] %in% c("41", "51", "54", "92")) {
    yt <- starpkodi5_11_50_51(y, t, prev, v)
  } else if (t$zinkod[3] %in% c("40", "50", "53", "91")) {
    yt <- starpkodi5_11_50_50(y, t, prev, v)
  } else {stop("Starpkodi5_11_50: Trūkst izstrādes koda.")}
  
  rm(y, t, prev, v)
  return(yt)
}   

#  } else if (t$zinkod[3] == "51" && t$zinkod[4] == "21" && t$zinkod[5] == "50" && 
#             all(diff(t$NDZ_sanemsanas_datums[1:4]) != 0) && all(diff(t$NDZ_sanemsanas_datums[4:5]) == 0)) {
#    days1 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[2], t$NDZ_sanemsanas_datums[1], units = "days"))
#    days2 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[4], t$NDZ_sanemsanas_datums[3], units = "days"))
#    
#    yt <- y[v, ]
#    yt$dienas <- sum(days1, days2)
#    rm(days1, days2)
#
#  } else if (t$zinkod[3] == "25" && all(t$zinkod[4:5] == "51") && diff(t$NDZ_sanemsanas_datums[3:4]) == 0 &&
#             all(sapply(c(1,2,4), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
#    yt <- y[v, ]
#    yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[2], t$NDZ_sanemsanas_datums[1], units = "days"))
#  } else if (t$zinkod[3] == "11" && t$zinkod[4] == "50" && t$zinkod[5] == "51" && 
#             all(sapply(seq(2,5,by=2), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0)) &&
#             all(sapply(seq(1,4,by=2), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0))) {
#    yt <- y[v, ]
#    yt$dienas <- as.numeric(difftime(t$last_date[5], t$NDZ_sanemsanas_datums[5], units = "days")) + 1
#  } else if (t$zinkod[3] == "25" && t$zinkod[4] == "50" && t$zinkod[5] == "51" && 
#             diff(t$NDZ_sanemsanas_datums[2:3]) != 0 &&
#             all(sapply(c(1,3,4), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0))) {
#    yt <- y[v, ]
#    yt$dienas <- 0
#  } else {stop("Starpkodi5_11_50: Trūkst izstrādes koda.")}
  

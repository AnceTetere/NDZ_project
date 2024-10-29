starpkodi4_11_50 <- function(y, t, prev, v) {
  
  if(t$zinkod[3] == "51") {
    yt <- starpkodi4_11_50_51(y, t, prev, v)
  } else if (t$zinkod[3] %in% c("21", "22", "23", "24", "25", "29")) {
    yt <- starpkodi4_11_50_25(y, t, prev, v)
#  } else if (t$zinkod[3] == "50" && t$zinkod[4] == "51" && 
#             all(sapply(c(1,3), function(i) all(diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) &&
#             all(diff(t$NDZ_sanemsanas_datums[2:3]) == 0)) {
#    days1 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[2], t$NDZ_sanemsanas_datums[1], units = "days"))
#    days2 <- as.numeric(difftime(t$last_date[4], t$NDZ_sanemsanas_datums[4], units = "days")) + 1
#    
#    yt <- y[v, ]
#    yt$dienas <- sum(days1, days2)
#    rm(days1, days2)
#   } else if (t$zinkod[3] == "50" && t$zinkod[4] == "51" && 
#             all(sapply(c(1,3), function(i) all(diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) &&
#             all(diff(t$NDZ_sanemsanas_datums[2:3]) == 0)) {
#    days1 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[2], t$NDZ_sanemsanas_datums[1], units = "days"))
#    days2 <- as.numeric(difftime(t$last_date[4], t$NDZ_sanemsanas_datums[4], units = "days")) + 1
#    
#    yt <- y[v, ]
#    yt$dienas <- sum(days1, days2)
#    rm(days1, days2)
#  } else if (t$zinkod[3] == "11" && t$zinkod[4] == "51" && 
#             all(sapply(seq(1,4,by=2), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0))&&
#             diff(t$NDZ_sanemsanas_datums[2:3]) != 0) {
#    yt <- y[v, ]
#    yt$dienas <- as.numeric(difftime(t$last_date[2], t$NDZ_sanemsanas_datums[4], units = "days")) + 1
#  } else if (t$zinkod[3] == "50" && t$zinkod[4] == "51" && all(diff(t$NDZ_sanemsanas_datums) != 0)) {
#    days1 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[3], t$NDZ_sanemsanas_datums[1], units = "days"))
#    days2 <- as.numeric(difftime(t$last_date[4], t$NDZ_sanemsanas_datums[4], units = "days")) + 1
#    
#    yt <- y[v, ]
#    yt$dienas <- sum(days1, days2)
#    rm(days1, days2)
  } else {stop("Starpkodi4_11_50: Trūkst izstrādes koda.")}
  
  return(yt) 
}

starpkodi3_11 <- function(y, t, prev, v) {
  
  if (t$zinkod[2] == "50"){
    yt <- starpkodi3_11_50(y, t, prev, v)
  } else if (t$zinkod[2] == "40"){
    yt <- starpkodi3_11_40(y, t, prev, v)
  } else {stop("Starpkodi3_11: Trūkst izstrādes koda.")}
  
#  } else if (t$zinkod[2] == "53") {
#    yt <- starpkodi3_11_53(y, t, prev, v)
#  } else if (t$zinkod[2] %in% c("21", "25")) {
#    yt <- starpkodi3_11_25(y, t, prev, v)
#  } else if (t$zinkod[2] %in% c("51", "41")) {
#    yt <- starpkodi3_11_51(y, t, prev, v)
#  } else 
  
#  } else if (t$zinkod[2] == "91" && t$zinkod[3] == "92" && all(!diff(t$NDZ_sanemsanas_datums) == 0)) {
#    days1 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[2], t$NDZ_sanemsanas_datums[1], units = "days"))
#    days2 <- as.numeric(difftime(t$last_date[3], t$NDZ_sanemsanas_datums[3], units = "days")) + 1 
#    
#    yt <- y[v, ]
#    yt$dienas <- sum(days1, days2)
#    rm(days1, days2)
#  } else if ((t$zinkod[2] == "91" && t$zinkod[3] == "92" && all(diff(t$NDZ_sanemsanas_datums[2:3]) != 0) && all(diff(t$NDZ_sanemsanas_datums[1:2]) == 0)) 
#             || 
#             (t$zinkod[2] == "40" && t$zinkod[3] == "41" && all(diff(t$NDZ_sanemsanas_datums[2:3]) != 0) && all(diff(t$NDZ_sanemsanas_datums[1:2]) == 0))) {
#    yt <- y[v, ]
#    yt$dienas <- as.numeric(difftime(t$last_date[3], t$NDZ_sanemsanas_datums[3], units = "days")) + 1 
#  } else if (t$zinkod[2] == "11" && t$zinkod[3] == "91" && all(diff(t$NDZ_sanemsanas_datums) != 0)) {
#    yt <- y[v, ]
#    yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[3], t$NDZ_sanemsanas_datums[2], units = "days"))
# 

#  } else if (t$zinkod[2] == "11" && t$zinkod[3] == "50" && all(diff(t$NDZ_sanemsanas_datums) != 0)) {
#    yt <- y[v, ]
#    yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[3], t$NDZ_sanemsanas_datums[2], units = "days"))
#  } else if (t$zinkod[2] == "11" && t$zinkod[3] %in% c("40", "50")) {
#    yt <- y[v, ]
#    yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[3], t$NDZ_sanemsanas_datums[2], units = "days"))

#  } else if (t$zinkod[2] == "91" && t$zinkod[3] == "40" && all(diff(t$NDZ_sanemsanas_datums) == 0)) {
#    yt <- y[v, ]
#    yt$dienas <- 0
  
  return(yt) 
}

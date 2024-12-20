starpkodi5_50 <- function(y, t, prev, v) {
  
  if (t$zk[2] == "51") {
    yt <- starpkodi5_50_51(y, t, prev, v)
  } else if(t$zk[2] == "25" && 
          t$zk[3] == "51" && t$zk[4] == "11" && 
          t$zk[5] == "25" && 
          all(!diff(t$NDZ_sanemsanas_datums[3:5]) == 0) &&
          t$NDZ_sanemsanas_datums[2] == t$NDZ_sanemsanas_datums[3] &&
          t$NDZ_sanemsanas_datums[1] != t$NDZ_sanemsanas_datums[2]) {
  days1 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) 
  days2 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[5], t$NDZ_sanemsanas_datums[4], units = "days")) + 1 
  
  yt <- y[v, ]
  yt$dienas <- sum(days1, days2)
  rm(days1, days2)
} else if(all(t$zk[1:3] == "50") && t$zk[5] == "51" && t$zk[4] == "25" && 
          all(diff(t$NDZ_sanemsanas_datums[1:4]) != 0) && diff(t$NDZ_sanemsanas_datums[4:5]) == 0) {
  yt <- y[v, ]
  yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[3], prev, units = "days"))
} else if(t$zk[2] == "21" && all(t$zk[3:5] == "51")) {
  yt <- y[v, ]
  yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1 
} else if(t$zk[2] == "91" && t$zk[3] == "51" && t$zk[4] == "92" && t$zk[5] == "50" &&
          diff(t$NDZ_sanemsanas_datums[2:3]) == 0 &&
          all(sapply(c(1,3,4), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
  days1 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1 
  days2 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[2], t$NDZ_sanemsanas_datums[3], units = "days"))
  days3 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[5], t$NDZ_sanemsanas_datums[4], units = "days"))
  
  yt <- y[v, ]
  yt$dienas <- sum(days1, days2, days3) 
  rm(days1, days2, days3)
} else {stop("Starpkodi5_50: iztrūkst apstrādes koda.")}

  if(is.na(yt$PS_code[1])) {stop("DD NA.")}
return(yt)
}

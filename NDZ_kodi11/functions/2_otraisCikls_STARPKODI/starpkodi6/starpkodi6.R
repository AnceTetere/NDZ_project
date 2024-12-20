starpkodi6 <- function(y, t, prev, v) {
  
  if (t$zk[1] %in% c("11", "14", "16", "61")) {
    yt <- starpkodi6_11(y, t, prev, v)
 # } else if (t$zk[1] == "51") {
 #   yt <- starpkodi6_51(y, t, prev, v)
 # } else if (t$zk[1] == "50") {
 #   yt <- starpkodi6_50(y, t, prev, v)
 # } else if (t$zk[1] == "26") {
 #   yt <- starpkodi6_26(y, t, prev, v)
 # } else 
 } else if (t$zk[1] %in% c("40", "50", "53", "91")) {
    yt <- starpkodi6_91(y, t, prev, v)
    } else {stop("Starpkodi6 iztrūkst apstrādes koda.")}
  
 # } else if (t$zk[1] == "40" && 
 #            t$zk[2] == "50" && 
 #            t$zk[3] == "41" && 
 #            t$zk[4] == "51" && 
 #            t$zk[5] == "91" && 
 #            t$zk[6] == "92" && 
 #            all(diff(t$NDZ_sanemsanas_datums[1:2]) == 0) && all(diff(t$NDZ_sanemsanas_datums[3:4]) == 0) &&
 #            all(diff(t$NDZ_sanemsanas_datums[4:6]) != 0) && all(diff(t$NDZ_sanemsanas_datums[2:3]) != 0)) {
 #   yt <- y[v:(v+1), ]
 #   yt <- yt[yt$zk == "40", ]

 # } else if (t$zk[1] == "41" && 
 #            t$zk[2] == "50" && 
 #            t$zk[3] == "51" && 
 #            t$zk[4] == "40" && 
 #            t$zk[5] == "41" && 
 #            t$zk[6] == "50" && 
 #            all(diff(t$NDZ_sanemsanas_datums[1:2]) == 0) && all(diff(t$NDZ_sanemsanas_datums[2:5]) != 0) &&
 #            all(diff(t$NDZ_sanemsanas_datums[5:6]) == 0)) {
  #  yt <- y[v, ]
  #  yt$dienas <- as.numeric(difftime(t$beidz[4], t$sak[3], units = "days"))
  #} else if (t$zk[1] == "53" && t$zk[2] == "53" && 
  #           t$zk[3] == "54" && t$zk[4] == "54" && 
  #           t$zk[5] == "53" && t$zk[6] == "21" && 
  #           all(diff(t$NDZ_sanemsanas_datums[2:3]) == 0) && all(diff(t$NDZ_sanemsanas_datums[1:2]) != 0) &&
  #           all(diff(t$NDZ_sanemsanas_datums[3:6]) != 0)) {
  #  days1 <- as.numeric(difftime(t$beidz[1], prev, units = "days")) - 1 #jo dīkstāve
  #  days2 <- as.numeric(difftime(t$beidz[5], t$sak[4], units = "days"))
  #  
  #  yt <- y[v, ]
  #  yt$dienas <- sum(days1, days2)
  #  rm(days1, days2)
  #} else if (all(t$zk[c(1,3,5)] == "92") && all(t$zk[c(2,4)] == "91") && t$zk[6] == "21" && all(diff(t$NDZ_sanemsanas_datums) != 0)) {
  #  days <- 0
  #  for(d in seq(1,5,by=2)) {
  #    days <- days + as.numeric(difftime(t$beidz[d+1], t$sak[d], units = "days"))
  #  }
  #  
  #  yt <- y[v, ]
  #  yt$dienas <- days + 1
  #  rm(days, d)
  #} else if (all(t$zk[c(1,3,5)] == "53") && all(t$zk[c(2,4)] == "54") && t$zk[6] == "21" && all(diff(t$NDZ_sanemsanas_datums) != 0)) {
  #  days <- as.numeric(difftime(t$beidz[1], prev, units = "days")) - 1
  #  for(d in seq(2,4,by=2)) {
  #    days <- days + as.numeric(difftime(t$beidz[d+1], t$sak[d], units = "days"))
  #  }
  #  
  #  yt <- y[v, ]
  #  yt$dienas <- days
  #  rm(days, d)
  #} else if (t$zk[1] == "41" && t$zk[2] == "50" && t$zk[3] == "91" &&
  #           t$zk[4] == "51" && t$zk[5] == "40" && t$zk[6] == "92" &&
  #           all(sapply(seq(1,6,by=2), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
  #           all(sapply(seq(2,5,by=2), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
  #  yt <- y[v, ]
  #  yt$dienas <- 0
  #} else if (t$zk[1] == "21" && t$zk[2] == "11" && t$zk[3] == "50" &&
  #           t$zk[4] == "51" && t$zk[5] == "50" && t$zk[6] == "51" &&
  #           all(diff(t$NDZ_sanemsanas_datums) != 0)) {
  #  days1 <- as.numeric(difftime(t$beidz[1], prev, units = "days"))
  #  days2 <- sum(sapply(seq(2,5,by=2), function(i) as.numeric(difftime(t$beidz[i+1], t$sak[i], units = "days"))))
  #  days3 <- as.numeric(difftime(t$last_date[6], t$sak[6], units = "days")) + 1
  #  
  #  yt <- y[v, ]
  #  yt$dienas <- sum(days1, days2, days3)
  #  rm(days1, days2, days3)
  
  if(is.na(yt$PS_code[1])) {stop("DD NA.")}
  yt$zk <- "combined" 
  return(yt)
}

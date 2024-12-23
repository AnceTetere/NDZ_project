starpkodi6 <- function(y, t, prev, v) {
  
  if (t$zk[1] %in% c("11", "14", "16", "61")) {
    yt <- starpkodi6_11(y, t, prev, v)
  } else if (t$zk[1] %in% c("21", "22", "23", "24", "25", "29")) {
    yt <- starpkodi6_25(y, t, prev, v)
  } else if (t$zk[1] %in% c("41", "51", "54", "92")) {
    yt <- starpkodi6_51(y, t, prev, v)
 # } else if (t$zk[1] == "50") {
 #   yt <- starpkodi6_50(y, t, prev, v)
 # } else if (t$zk[1] == "26") {
 #   yt <- starpkodi6_26(y, t, prev, v)
 # } else 
 } else if (t$zk[1] %in% c("40", "50", "53", "91")) {
    yt <- starpkodi6_91(y, t, prev, v)
  } else {stop("Starpkodi6 iztrūkst apstrādes koda.")}
  
  #} else if (t$zk[1] == "21" && t$zk[2] == "11" && t$zk[3] == "50" &&
  #           t$zk[4] == "51" && t$zk[5] == "50" && t$zk[6] == "51" &&
  #           all(diff(t$NDZ_sanemsanas_datums) != 0)) {
  #  days1 <- as.numeric(difftime(t$beidz[1], prev, units = "days"))
  #  days2 <- sum(sapply(seq(2,5,by=2), function(i) as.numeric(difftime(t$beidz[i+1], t$sak[i], units = "days"))))
  #  days3 <- as.numeric(difftime(t$last_date[6], t$sak[6], units = "days")) + 1
  #  
  #  yt <- y[v, ]
  #  yt$dd <- sum(days1, days2, days3)
  #  rm(days1, days2, days3)
  
  if(is.na(yt$PS_code[1])) {stop("DD NA.")}
  yt$zk <- "combined"  #jo starpkodu dienu sarēķins
  return(yt)
}

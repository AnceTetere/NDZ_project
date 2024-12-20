starpkodi5 <- function(y, t, prev, v) {

    if (t$zk[1] == "11") {
    yt <- starpkodi5_11(y, t, prev, v)
  } else if (t$zk[1] == "91") {
    yt <- starpkodi5_91(y, t, prev, v)
  } else if (t$zk[1] == "50") {
    yt <- starpkodi5_50(y, t, prev, v) 
  } else if (t$zk[1] %in% c("41", "51", "54", "92")) {
    yt <- starpkodi5_51(y, t, prev, v)
  } else {stop("Starpkodi5: iztrūkst apstrādes koda.")}
    
    if(is.na(yt$PS_code[1])) {stop("DD NA.")}
    yt$zk <- "combined"
    return(yt)
}



## else if(t$zk[1] == "25" && t$zk[2] == "11" && 
##         t$NDZ_sanemsanas_datums[1] != t$NDZ_sanemsanas_datums[2] && 
##         t$NDZ_sanemsanas_datums[2] == t$NDZ_sanemsanas_datums[3] && 
##         t$zk[3] == "81" && t$zk[4] == "25" && 
##         t$NDZ_sanemsanas_datums[3] != t$NDZ_sanemsanas_datums[4] && 
##         t$zk[5] == "82" && 
##         t$NDZ_sanemsanas_datums[4] == t$NDZ_sanemsanas_datums[5]) {
## days1 <- as.numeric(difftime(as.Date(t$NDZ_sanemsanas_datums[1]), prev, units = "days"))
## days2 <- as.numeric(difftime(as.Date(t$NDZ_sanemsanas_datums[5]), as.Date(t$NDZ_sanemsanas_datums[3]), units = "days")) + 1 
## days <- days1 + days2
## rm(days1, days2)
## 
## yt <- y[v, ]
## yt$dienas <- days
## else if(t$zk[1] == "25" && t$zk[2] == "25" && 
##         t$zk[3] == "11" && t$zk[4] == "81" && 
##         t$zk[5] == "82" && t$NDZ_sanemsanas_datums[1] != t$NDZ_sanemsanas_datums[2] &&
##         all(diff(t$NDZ_sanemsanas_datums[2:5]) == 0)) {
## days1 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) 
## days2 <- 1 
## days <- days1 + days2
## rm(days1, days2)
## 
## yt <- y[v, ]
## yt$dienas <- days
## else if(t$zk[1] == "25" && t$zk[2] == "26" && 
##         t$zk[3] == "11" && t$zk[4] == "11" && 
##         t$zk[5] == "81" && 
##         t$NDZ_sanemsanas_datums[1] != t$NDZ_sanemsanas_datums[2] &&
##         t$NDZ_sanemsanas_datums[2] == t$NDZ_sanemsanas_datums[3] &&
##         t$NDZ_sanemsanas_datums[4] == t$NDZ_sanemsanas_datums[5]) {
## days1 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) 
## days2 <- 0 #darba neuzsākšana
## days3 <- as.numeric(difftime(t$last_date[5], t$NDZ_sanemsanas_datums[5], units = "days"))
## 
## days <- days1 + days2 + days3
## rm(days1, days2, days3)
## 
## yt <- y[v, ]
## yt$dienas <- days
## else if(t$zk[1] == "25" && t$zk[2] == "11" && 
##         t$zk[3] == "81" && t$zk[4] == "82" && 
##         t$zk[5] == "11" && 
##         all(diff(t$NDZ_sanemsanas_datums[1:4]) == 0) &&
##         t$NDZ_sanemsanas_datums[4] != t$NDZ_sanemsanas_datums[5]) {
## days1 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[4], t$NDZ_sanemsanas_datums[3], units = "days")) + 1 
## days2 <- as.numeric(difftime(t$last_date[5], t$NDZ_sanemsanas_datums[5], units = "days")) +1  
## 
## yt <- y[v, ]
## yt$dienas <- sum(days1, days2)
## rm(days1, days2)
## else if(all(t$zk[c(1,4)] == "25") && t$zk[2] == "51" && all(t$zk[c(3,5)] == "11") && 
##         all(diff(t$NDZ_sanemsanas_datums[2:5]) != 0) && all(diff(t$NDZ_sanemsanas_datums[1:2]) == 0)) {
## days1 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[1], t$NDZ_sanemsanas_datums[2], units = "days"))
## days2 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[4], t$NDZ_sanemsanas_datums[3], units = "days")) 
## days3 <- as.numeric(difftime(t$last_date[5], t$NDZ_sanemsanas_datums[5], units = "days")) + 1  
## 
## yt <- y[v, ]
## yt$dienas <- sum(days1, days2, days3)

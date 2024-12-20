starpkodi6_11 <- function(y2, t, prev, v) {
  
if (t$zk[2] == "26" && 
             t$zk[3] == "11" && 
             t$zk[4] == "50" && 
             t$zk[5] == "51" && 
             t$zk[6] == "25" && 
             all(diff(t$NDZ_sanemsanas_datums[1:2]) == 0) &&
             all(diff(t$NDZ_sanemsanas_datums[2:6]) != 0)) {
    days1 <- as.numeric(difftime(t$beidz[4], t$sak[3], units = "days"))
    days2 <- as.numeric(difftime(t$beidz[6], t$sak[5], units = "days")) + 1 
    
    yt <- y2[v, ]
    yt$dienas <- sum(days1, days2)
    rm(days1, days2)
} else if (t$zk[2] %in% c("40", "50", "53", "91")) {
  yt <- starpkodi6_11_50(y2, t, prev, v)
} else if (all(t$zk[seq(1,6,by=2)] == "11") && all(t$zk[c(2,4)] == "25") && t$zk[6] == "50" && 
       all(diff(t$NDZ_sanemsanas_datums) != 0)) {
    yt <- y2[v, ]
    yt$dienas <- sum(sapply(seq(1,6,by=2), function(i) as.numeric(difftime(t$beidz[i+1], t$sak[i], units = "days")) + 1)) - 1 
  } else {stop("starpkodi6_11: Trūkst izstrādes koda starpkodi2.")}
  
  if(is.na(yt$PS_code[1])) {stop("DD NA.")}
  return(yt)
}

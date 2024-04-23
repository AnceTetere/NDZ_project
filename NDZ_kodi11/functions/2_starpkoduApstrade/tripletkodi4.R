tripletkodi4 <- function(y3, t, prev, v) {
  
  if (t$zinkod[1] == "41" && t$zinkod[2] == "50" && 
             t$zinkod[3] == "25" && t$zinkod[4] == "51" && 
             t$NDZ_sanemsanas_datums[1] == t$NDZ_sanemsanas_datums[2] &&
             t$NDZ_sanemsanas_datums[2] != t$NDZ_sanemsanas_datums[3] &&
             t$NDZ_sanemsanas_datums[3] == t$NDZ_sanemsanas_datums[4]) {

    yt <- y3[v, ]
    yt$dienas <- 0
  } else if (t$zinkod[1] == "41" && t$zinkod[2] == "50" && 
       t$zinkod[3] == "51" && t$zinkod[4] == "25" && all(diff(t$NDZ_sanemsanas_datums) != 0)) {
    days1 <- as.numeric(difftime(t$beidz[2], t$sak[1], units = "days"))
    days2 <- as.numeric(difftime(t$beidz[4], t$sak[3], units = "days"))
    
    yt <- y3[v, ]
    yt$dienas <- sum(days1, days2)
    rm(days1, days2)
  } else {
    stop("Tripletkodi4 iztrūkst apstrādes koda.")
  }
  
  if(is.na(yt$PS_code[1])) {stop("Dienas NA.")}
  yt$zinkod <- "combined"  #jo tripletkodu dienu sarēķins
  return(yt)
}

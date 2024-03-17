starpkodi2 <- function(y2, t) {
  if (t$zinkod[1] == "51" &&
      t$zinkod[2] == "25" &&
      t$ZDN_sanemsanas_datums[1] <= t$ZDN_sanemsanas_datums[2]) {
    yt <- y2[v:(v + 1),]
    days <-
      as.numeric(difftime(t$beidz_darbu[2], t$sak_darbu[1], units = "days")) + 1
        yt <- yt[1,]
    yt$dienas <- days
  } else if (t$zinkod[1] == "25" && t$zinkod[2] == "51" &&
             t$ZDN_sanemsanas_datums[1] == t$ZDN_sanemsanas_datums[2]) {
    yt <- y2[v,]
    yt$dienas <- 0
  } else if (t$zinkod[1] == "11" && t$zinkod[2] == "50" &&
             t$ZDN_sanemsanas_datums[1] < t$ZDN_sanemsanas_datums[2]) {
    yt <- y2[v:(v + 1),]
    days <-
      as.numeric(difftime(t$beidz_darbu[2], t$sak_darbu[1], units = "days"))
    yt <- yt[1,]
    yt$dienas <- days
  } else if (t$zinkod[1] == "50" && t$zinkod[2] == "25") {
    yt <- y2[v:(v + 1),]
    yt <- yt[yt$zinkod == "50",]
  } else if (t$zinkod[1] == "51" && t$zinkod[2] == "21") {
    yt <- y2[v:(v + 1),]
    days <-
      as.numeric(difftime(as.Date(t$beidz_darbu[2]), as.Date(t$sak_darbu)[1], units = "days"))
    yt <- yt[1,]
    yt$dienas <- days
  } else if (t$zinkod[1] == "54" && t$zinkod[2] == "21") {
    yt <- y2[v:(v + 1),]
    days <-
      as.numeric(difftime(as.Date(t$beidz_darbu[2]), as.Date(t$sak_darbu)[1], units = "days")) + 1
    yt <- yt[1,]
    yt$dienas <- days
  } else if (t$zinkod[1] == "21" && t$zinkod[2] == "51" &&
             t$ZDN_sanemsanas_datums[1] == t$ZDN_sanemsanas_datums[2]) {
    yt <- y2[v,]
    yt$dienas <- 0
  } else if (t$zinkod[1] == "11" && t$zinkod[2] == "50" &&
             t$ZDN_sanemsanas_datums[1] == t$ZDN_sanemsanas_datums[2]) {
    yt <- y2[v,]
    yt$dienas <- 0
  } else if (t$zinkod[1] == "21" && t$zinkod[2] == "53" &&
             t$ZDN_sanemsanas_datums[1] == t$ZDN_sanemsanas_datums[2]){
    yt <- y2[v:(v + 1),]
    yt <- yt[yt$zinkod == "11",]
  } else if (t$zinkod[1] == "51" && t$zinkod[2] == "23" &&
            t$ZDN_sanemsanas_datums[1] != t$ZDN_sanemsanas_datums[2]) {
    days <- as.numeric(difftime(as.Date(t$beidz_darbu)[2], as.Date(t$sak_darbu)[1], units = "days"))
    yt <- y2[v,]
    yt$dienas <- days
  } else if (t$zinkod[1] == "53" && t$zinkod[2] == "21" &&
            t$ZDN_sanemsanas_datums[1] != t$ZDN_sanemsanas_datums[2]) {
    yt <- y2[v:(v+1),] 
    yt <- yt[yt$zinkod == "53", ]
  } else if (t$zinkod[1] == "25" && t$zinkod[2] == "41" &&
             t$ZDN_sanemsanas_datums[1] == t$ZDN_sanemsanas_datums[2]) {
    yt <- y2[v,] 
    yt$dienas <- 0
  } else if (t$zinkod[1] == "41" && t$zinkod[2] == "50" &&
            t$ZDN_sanemsanas_datums[1] == t$ZDN_sanemsanas_datums[2]) {
    yt <- y2[v,] 
    yt$dienas <- 0
  } else if (t$zinkod[1] == "92" && t$zinkod[2] == "40" &&
             t$ZDN_sanemsanas_datums[1] != t$ZDN_sanemsanas_datums[2]) {
    days <- as.numeric(difftime(as.Date(t$beidz_darbu)[2], as.Date(t$sak_darbu)[1], units = "days"))
    yt <- y2[v,] 
    yt$dienas <- days
  } else if (t$zinkod[1] == "50" && t$zinkod[2] == "82" &&
            t$ZDN_sanemsanas_datums[1] != t$ZDN_sanemsanas_datums[2]) {
    yt <- y2[v:(v+1),] 
    yt <- yt[yt$zinkod == "50", ]
  } else if (t$zinkod[1] == "53" && t$zinkod[2] == "25" &&
             t$ZDN_sanemsanas_datums[1] != t$ZDN_sanemsanas_datums[2]) {
    yt <- y2[v:(v+1),] 
    yt <- yt[yt$zinkod == "53", ]
  } else if (t$zinkod[1] == "25" && t$zinkod[2] == "50" &&
             t$ZDN_sanemsanas_datums[1] == t$ZDN_sanemsanas_datums[2]) {
    yt <- y2[v:(v+1),] 
    yt <- yt[yt$zinkod == "50", ]
  } else if (t$zinkod[1] == "25" && t$zinkod[2] == "40" &&
              t$ZDN_sanemsanas_datums[1] == t$ZDN_sanemsanas_datums[2]) {
    yt <- y2[v:(v+1),] 
    yt <- yt[yt$zinkod == "40", ]
  } else if (t$zinkod[1] == "50" && t$zinkod[2] == "21" &&
             t$ZDN_sanemsanas_datums[1] != t$ZDN_sanemsanas_datums[2]) {
    yt <- y2[v:(v+1),] 
    yt <- yt[yt$zinkod == "50", ]
  } else if (t$zinkod[1] == "21" && t$zinkod[2] == "41" &&
             t$ZDN_sanemsanas_datums[1] == t$ZDN_sanemsanas_datums[2]) {
    yt <- y2[v,] 
    yt$dienas <- 0
  } else {
    stop("Trūkst izstrādes koda starpkodi2.")
  }
  
  yt$zinkod <- "combined"  #jo starpkodu dienu sarēķins
  return(yt)
  rm(days, yt)
}

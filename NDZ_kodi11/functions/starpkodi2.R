starpkodi2 <- function(y2, t) {
  if (t$zinkod[1] == "51" &&
      t$zinkod[2] == "25" &&
      t$NDZ_sanemsanas_datums[1] <= t$NDZ_sanemsanas_datums[2]) {
    yt <- y2[v:(v + 1),]
    days <-
      as.numeric(difftime(t$beidz[2], t$sak[1], units = "days")) + 1
    yt <- yt[1,]
    yt$dienas <- days
  } else if (t$zinkod[1] == "25" && t$zinkod[2] == "51" &&
             t$NDZ_sanemsanas_datums[1] == t$NDZ_sanemsanas_datums[2]) {
    yt <- y2[v,]
    yt$dienas <- 0
  } else if (t$zinkod[1] == "11" && t$zinkod[2] == "50" &&
             t$NDZ_sanemsanas_datums[1] < t$NDZ_sanemsanas_datums[2]) {
    yt <- y2[v:(v + 1),]
    days <-
      as.numeric(difftime(t$beidz[2], t$sak[1], units = "days"))
    yt <- yt[1,]
    yt$dienas <- days
  } else if (t$zinkod[1] == "50" && t$zinkod[2] == "25") {
    yt <- y2[v:(v + 1),]
    yt <- yt[yt$zinkod == "50",]
  } else if (t$zinkod[1] == "51" && t$zinkod[2] == "21") {
    yt <- y2[v:(v + 1),]
    days <-
      as.numeric(difftime(as.Date(t$beidz[2]), as.Date(t$sak)[1], units = "days"))
    yt <- yt[1,]
    yt$dienas <- days
  } else if (t$zinkod[1] == "54" && t$zinkod[2] == "21") {
    yt <- y2[v:(v + 1),]
    days <-
      as.numeric(difftime(as.Date(t$beidz[2]), as.Date(t$sak)[1], units = "days")) + 1
    yt <- yt[1,]
    yt$dienas <- days
  } else if (t$zinkod[1] == "21" && t$zinkod[2] == "51" &&
             t$NDZ_sanemsanas_datums[1] == t$NDZ_sanemsanas_datums[2]) {
    yt <- y2[v,]
    yt$dienas <- 0
  } else if (t$zinkod[1] == "11" && t$zinkod[2] == "50" &&
             t$NDZ_sanemsanas_datums[1] == t$NDZ_sanemsanas_datums[2]) {
    yt <- y2[v,]
    yt$dienas <- 0
  } else if (t$zinkod[1] == "21" && t$zinkod[2] == "53" &&
             t$NDZ_sanemsanas_datums[1] == t$NDZ_sanemsanas_datums[2]){
    yt <- y2[v:(v + 1),]
    yt <- yt[yt$zinkod == "11",]
  } else if (t$zinkod[1] == "51" && t$zinkod[2] == "23" &&
            t$NDZ_sanemsanas_datums[1] != t$NDZ_sanemsanas_datums[2]) {
    days <- as.numeric(difftime(as.Date(t$beidz)[2], as.Date(t$sak)[1], units = "days"))
    yt <- y2[v,]
    yt$dienas <- days
  } else if (t$zinkod[1] == "53" && t$zinkod[2] == "21" &&
            t$NDZ_sanemsanas_datums[1] != t$NDZ_sanemsanas_datums[2]) {
    yt <- y2[v:(v+1),] 
    yt <- yt[yt$zinkod == "53", ]
  } else if (t$zinkod[1] == "25" && t$zinkod[2] == "41" &&
             t$NDZ_sanemsanas_datums[1] == t$NDZ_sanemsanas_datums[2]) {
    yt <- y2[v,] 
    yt$dienas <- 0
  } else if (t$zinkod[1] == "41" && t$zinkod[2] == "50" &&
            t$NDZ_sanemsanas_datums[1] == t$NDZ_sanemsanas_datums[2]) {
    yt <- y2[v,] 
    yt$dienas <- 0
  } else if (t$zinkod[1] == "92" && t$zinkod[2] == "40" &&
             t$NDZ_sanemsanas_datums[1] != t$NDZ_sanemsanas_datums[2]) {
    days <- as.numeric(difftime(as.Date(t$beidz)[2], as.Date(t$sak)[1], units = "days"))
    yt <- y2[v,] 
    yt$dienas <- days
  } else if (t$zinkod[1] == "50" && t$zinkod[2] == "82" &&
            t$NDZ_sanemsanas_datums[1] != t$NDZ_sanemsanas_datums[2]) {
    yt <- y2[v:(v+1),] 
    yt <- yt[yt$zinkod == "50", ]
  } else if (t$zinkod[1] == "53" && t$zinkod[2] == "25" &&
             t$NDZ_sanemsanas_datums[1] != t$NDZ_sanemsanas_datums[2]) {
    yt <- y2[v:(v+1),] 
    yt <- yt[yt$zinkod == "53", ]
  } else if (t$zinkod[1] == "25" && t$zinkod[2] == "50" &&
             t$NDZ_sanemsanas_datums[1] == t$NDZ_sanemsanas_datums[2]) {
    yt <- y2[v:(v+1),] 
    yt <- yt[yt$zinkod == "50", ]
  } else if (t$zinkod[1] == "25" && t$zinkod[2] == "40" &&
              t$NDZ_sanemsanas_datums[1] == t$NDZ_sanemsanas_datums[2]) {
    yt <- y2[v:(v+1),] 
    yt <- yt[yt$zinkod == "40", ]
  } else if (t$zinkod[1] == "50" && t$zinkod[2] == "21" &&
             t$NDZ_sanemsanas_datums[1] != t$NDZ_sanemsanas_datums[2]) {
    yt <- y2[v:(v+1),] 
    yt <- yt[yt$zinkod == "50", ]
  } else if (t$zinkod[1] == "21" && t$zinkod[2] == "41" &&
             t$NDZ_sanemsanas_datums[1] == t$NDZ_sanemsanas_datums[2]) {
    yt <- y2[v,] 
    yt$dienas <- 0
  } else if (t$zinkod[1] == "41" && t$zinkod[2] == "25" &&
             t$NDZ_sanemsanas_datums[1] != t$NDZ_sanemsanas_datums[2]) {
    days <- as.numeric(difftime(as.Date(t$beidz[2]), as.Date(t$sak[1]), units = "days"))
    yt <- y2[v,] 
    yt$dienas <- days
  } else if (t$zinkod[1] == "11" && t$zinkod[2] == "81" &&
             t$NDZ_sanemsanas_datums[1] <= t$NDZ_sanemsanas_datums[2]) {
    yt <- y2[v:(v+1),] 
    yt <- yt[yt$zinkod == "11", ]
  } else if (t$zinkod[1] == "25" && t$zinkod[2] == "92" &&
             t$NDZ_sanemsanas_datums[1] == t$NDZ_sanemsanas_datums[2]) {
    yt <- y2[v,] 
    yt$dienas <- 0
  } else if (t$zinkod[1] == "41" && t$zinkod[2] == "21" &&
            t$NDZ_sanemsanas_datums[1] <= t$NDZ_sanemsanas_datums[2]) {
    days <- as.numeric(difftime(as.Date(t$beidz[2]), as.Date(t$sak[1]), units = "days")) + 1
    
    yt <- y2[v,] 
    yt$dienas <- days
  } else if (t$zinkod[1] == "41" && t$zinkod[2] == "50" &&
             t$NDZ_sanemsanas_datums[1] <= t$NDZ_sanemsanas_datums[2]) {
    days <- as.numeric(difftime(as.Date(t$beidz[2]), as.Date(t$sak[1]), units = "days")) + 1
    
    yt <- y2[v,] 
    yt$dienas <- days
  } else if (t$zinkod[1] == "51" && t$zinkod[2] == "22" &&
             t$NDZ_sanemsanas_datums[1] <= t$NDZ_sanemsanas_datums[2]) {
    days <- as.numeric(difftime(as.Date(t$beidz[2]), as.Date(t$sak[1]), units = "days")) + 1
    yt <- y2[v,] 
    yt$dienas <- days
  } else if (t$zinkod[1] == "11" && t$zinkod[2] == "40" &&
            t$NDZ_sanemsanas_datums[1] == t$NDZ_sanemsanas_datums[2]) {
    yt <- y2[v,] 
    yt$dienas <- 0
  } else if (t$zinkod[1] == "53" && t$zinkod[2] == "82" &&
             t$NDZ_sanemsanas_datums[1] == t$NDZ_sanemsanas_datums[2]) {
    yt <- y2[v:(v+1),] 
    yt <- yt[yt$zinkod == "53", ]
  } else if (t$zinkod[1] == "11" && t$zinkod[2] == "91" &&
             t$NDZ_sanemsanas_datums[1] != t$NDZ_sanemsanas_datums[2]) {
    days <- as.numeric(difftime(as.Date(t$beidz[2]), as.Date(t$sak[1]), units = "days"))
    yt <- y2[v,] 
    yt$dienas <- days
  } else if (t$zinkod[1] == "54" && t$zinkod[2] == "25" &&
            t$NDZ_sanemsanas_datums[1] != t$NDZ_sanemsanas_datums[2]) {
    yt <- y2[v,] 
    yt$dienas <- as.numeric(difftime(as.Date(t$beidz[2]), as.Date(t$sak[1]), units = "days")) + 1 #jo darbs
  } else if (t$zinkod[1] == "25" && t$zinkod[2] == "53" &&
             t$NDZ_sanemsanas_datums[1] == t$NDZ_sanemsanas_datums[2]) {
    yt <- y2[v:(v+1),] 
    yt <- yt[yt$zinkod == "53", ]
  } else if (t$zinkod[1] == "26" && t$zinkod[2] == "82" &&
             t$NDZ_sanemsanas_datums[1] == t$NDZ_sanemsanas_datums[2]) {
    yt <- y2[v,] 
    yt$dienas <- 0
  } else if (t$zinkod[1] == "11" && t$zinkod[2] == "40" &&
             t$NDZ_sanemsanas_datums[1] != t$NDZ_sanemsanas_datums[2]) {
    yt <- y2[v,] 
    yt$dienas <- as.numeric(difftime(as.Date(t$beidz[2]), as.Date(t$sak[1]), units = "days"))
  } else if (t$zinkod[1] == "92" && t$zinkod[2] == "25" &&
             t$NDZ_sanemsanas_datums[1] != t$NDZ_sanemsanas_datums[2]) {
    yt <- y2[v,] 
    yt$dienas <- as.numeric(difftime(as.Date(t$beidz[2]), as.Date(t$sak[1]), units = "days")) + 1 #jo darbs
  } else if (t$zinkod[1] == "21" && t$zinkod[2] == "54" &&
            t$NDZ_sanemsanas_datums[1] == t$NDZ_sanemsanas_datums[2]) {
    yt <- y2[v,] 
    yt$dienas <- 0
  } else if (t$zinkod[1] == "22" && t$zinkod[2] == "51" &&
             t$NDZ_sanemsanas_datums[1] == t$NDZ_sanemsanas_datums[2]) {
    yt <- y2[v,] 
    yt$dienas <- 0
  } else {
    stop("Trūkst izstrādes koda starpkodi2.")
  }
  
  yt$zinkod <- "combined"  #jo starpkodu dienu sarēķins
  return(yt)
  rm(days, yt)
}

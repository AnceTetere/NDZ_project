starpkodi2 <- function(y2, t, prev, v) {
  
  if (t$NDZ_sanemsanas_datums[1] == t$NDZ_sanemsanas_datums[2]) {
    yt <- starpkodi2_vienadiDatumi(y2, t, prev, v)
  } else if (t$zinkod[1] == "51") {
    yt <- starpkodi2_51(y2, t, prev, v)
  } else if (t$zinkod[1] == "53") {
    yt <- starpkodi2_53(y2, t, prev, v)
  } else if (t$zinkod[1] == "50") {
    yt <- starpkodi2_50(y2, t, prev, v)
  } else if (t$zinkod[1] == "25") {
    yt <- starpkodi2_25(y2, t, prev, v)
  } else if (t$zinkod[1] == "11") {
    yt <- starpkodi2_11(y2, t, prev, v)
  } else if (t$zinkod[1] == "54" && t$zinkod[2] == "21") {
    yt <- y2[v,]
    yt$dienas <- as.numeric(difftime(t$beidz[2], t$sak[1], units = "days")) + 1
  } else if (t$zinkod[1] == "92" && t$zinkod[2] == "40") {
    yt <- y2[v,] 
    yt$dienas <- as.numeric(difftime(t$beidz[2], t$sak[1], units = "days"))
  } else if (t$zinkod[1] == "41" && t$zinkod[2] == "25") {
    yt <- y2[v,] 
    yt$dienas <- as.numeric(difftime(t$beidz[2], t$sak[1], units = "days"))
  } else if (t$zinkod[1] == "41" && t$zinkod[2] == "21") {
    yt <- y2[v,] 
    yt$dienas <- as.numeric(difftime(t$beidz[2], t$sak[1], units = "days")) + 1
  } else if (t$zinkod[1] == "41" && t$zinkod[2] == "50") {
    yt <- y2[v,] 
    yt$dienas <- as.numeric(difftime(t$beidz[2], t$sak[1], units = "days")) + 1
  } else if (t$zinkod[1] == "54" && t$zinkod[2] == "25") {
    yt <- y2[v,] 
    yt$dienas <- as.numeric(difftime(t$beidz[2], t$sak[1], units = "days")) + 1 
  } else if (t$zinkod[1] == "92" && t$zinkod[2] == "25") {
    yt <- y2[v,] 
    yt$dienas <- as.numeric(difftime(t$beidz[2], t$sak[1], units = "days")) + 1 
  } else if (t$zinkod[1] == "21" && t$zinkod[2] == "54") {
    yt <- y2[v,] 
    yt$dienas <- 0
  } else if (t$zinkod[1] == "40" && t$zinkod[2] == "25") {
    yt <- y2[v:(v+1),] 
    yt <- yt[yt$zinkod == "40", ]
  } else if (t$zinkod[1] == "92" && t$zinkod[2] == "21") {
    yt <- y2[v,] 
    yt$dienas <- 0 #bija atvaļinājumā un palika nesācis
  } else if (t$zinkod[1] == "40" && t$zinkod[2] == "50" &&
             t$PS_code[1] == '_______' && t$NM_code[1] == '__________') {
    yt <- y2[v,] 
    yt$dienas <- as.numeric(difftime(t$beidz[1], prev, units = "days")) - 1 
  } else if (t$zinkod[1] == "40" && t$zinkod[2] == "23") {
    yt <- y2[v,] 
    yt$dienas <- as.numeric(difftime(t$beidz[1], prev, units = "days")) - 1 
  } else if (t$zinkod[1] == "91" && t$zinkod[2] == "21") {
    yt <- y2[v,] 
    yt$dienas <- as.numeric(difftime(t$beidz[1], prev, units = "days")) - 1 
  } else if (t$zinkod[1] == "92" && t$zinkod[2] == "50") {
    yt <- y2[v,] 
    yt$dienas <- as.numeric(difftime(t$beidz[2], t$sak[1], units = "days"))
  } else if (t$zinkod[1] == "21" && t$zinkod[2] == "51") {
    yt <- y2[v,] 
    yt$dienas <- 0
  } else if (t$zinkod[1] == "40" && t$zinkod[2] == "51" && t$PS_code[1] == 'PK5E4555B12' && t$NM_code[1] == '40003668284') {

    days1 <- as.numeric(difftime(t$beidz[1], prev, units = "days")) -1 
    days2 <- as.numeric(difftime(t$last_date[2], t$sak[2], units = "days")) + 1 
    
    yt <- y2[v,] 
    yt$dienas <- sum(days1, days2)
    rm(days1,days2)
  } else if (t$zinkod[1] == "21" && t$zinkod[2] == "41" && diff(t$NDZ_sanemsanas_datums) != 0) {
    yt <- y2[v,] 
    yt$dienas <- 0
  } else {
    stop("Starpkodi2: Trūkst izstrādes koda.")
  }
  
  if(is.na(yt$PS_code[1])) {stop("Dienas NA.")}
  yt$zinkod <- "combined"  #jo starpkodu dienu sarēķins
  return(yt)
}

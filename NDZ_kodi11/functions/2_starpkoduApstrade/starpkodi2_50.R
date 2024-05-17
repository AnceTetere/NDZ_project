starpkodi2_50 <- function(y2, t, prev, v) {
  
  if (t$zinkod[2] == "25") {
    yt <- y2[v:(v + 1),]
    yt <- yt[yt$zinkod == "50",]
  } else if (t$zinkod[2] == "21" && t$NDZ_sanemsanas_datums[1] != t$NDZ_sanemsanas_datums[2]) {
    yt <- y2[v:(v+1),] 
    yt <- yt[yt$zinkod == "50", ]
  } else if (t$zinkod[2] == "22" && t$NDZ_sanemsanas_datums[1] != t$NDZ_sanemsanas_datums[2]) {
    yt <- y2[v,] 
    yt$dienas <- as.numeric(difftime(t$beidz[1], prev, units = "days")) - 1 
  } else if (t$zinkod[2] == "24" && diff(t$NDZ_sanemsanas_datums) != 0) {
    yt <- y2[v,] 
    yt$dienas <- as.numeric(difftime(t$beidz[1], prev, units = "days")) - 1 
  } else if (t$zinkod[2] == "41" && diff(t$NDZ_sanemsanas_datums) != 0 &&
             t$PS_code[1] == '___________' && t$NM_code[1] == '_________') {
    days1 <- as.numeric(difftime(t$beidz[1], prev, units = "days")) - 1
    days2 <- as.numeric(difftime(t$last_date[2], t$sak[2], units = "days")) + 1
    
    yt <- y2[v,] 
    yt$dienas <- sum(days1, days2)
    rm(days1, days2)
  } else if (t$zinkod[2] == "53" && diff(t$NDZ_sanemsanas_datums) != 0 &&
             t$PS_code[1] == '___________' && t$NM_code[1] == '___________') {
    yt <- y2[v,] 
    yt$dienas <- as.numeric(difftime(t$beidz[2], prev, units = "days")) - 1
  } else {
    stop("Starpkodi2_50: Trūkst izstrādes koda.")
  }
  
  if(is.na(yt$PS_code[1])) {stop("Dienas NA.")}
  yt$zinkod <- "combined"  #jo starpkodu dienu sarēķins
  return(yt)
}

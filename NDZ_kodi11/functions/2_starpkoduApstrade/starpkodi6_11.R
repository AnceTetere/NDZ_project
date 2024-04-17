starpkodi6_11 <- function(y2, t, prev, v) {
  if (t$zinkod[2] == "25" && t$NDZ_sanemsanas_datums[1] <= t$NDZ_sanemsanas_datums[2] && t$zinkod[3] == "25" && t$zinkod[4] == "11" && t$zinkod[5] == "81" && t$zinkod[6] == "82" && all(t$NDZ_sanemsanas_datums[4:5] == t$NDZ_sanemsanas_datums[3])) {
    yt <- y2[v:(v+1), ]
    yt <- yt[yt$zinkod == "11", ]
  } else if (t$zinkod[2] == "25" && 
             t$zinkod[3] == "11" && 
             t$zinkod[4] == "25" && 
             t$zinkod[5] == "81" && 
             t$zinkod[6] == "82" && 
             all(diff(t$NDZ_sanemsanas_datums[3:6]) == 0) &&
             all(!diff(t$NDZ_sanemsanas_datums[1:3]) == 0)) {
    days <- as.numeric(difftime(as.Date(t$beidz[2]), as.Date(t$sak[1]), units = "days")) + 1 
    yt <- y2[v, ]
    yt$dienas <- days
  } else if (t$zinkod[2] == "26" && 
             t$zinkod[3] == "82" && 
             t$zinkod[4] == "81" && 
             t$zinkod[5] == "11" && 
             t$zinkod[6] == "81" && 
             all(diff(t$NDZ_sanemsanas_datums[1:4]) == 0)&&
             t$NDZ_sanemsanas_datums[5] == t$NDZ_sanemsanas_datums[6]) {
    days1 <- as.numeric(difftime(t$last_date[6], t$sak[6], units = "days")) + 1 
    days <- 1 + days1
    rm(days1)
    
    yt <- y2[v, ]
    yt$dienas <- days
  } else if (t$zinkod[2] %in% c("50", "53") && #abi ir beidz kodi 
             t$zinkod[3] %in% c("51", "54") && #abi ir sak kodi
             t$zinkod[4] %in% c("50", "53") && 
             t$zinkod[5] %in% c("51", "54") && 
             t$zinkod[6] %in% c("50", "21") && #abi ir beidz kodi
             all(!diff(t$NDZ_sanemsanas_datums) == 0)) {
    days1 <- as.numeric(difftime(t$beidz[2], t$sak[1], units = "days"))
    days2 <- as.numeric(difftime(t$beidz[4], t$sak[3], units = "days"))
    days3 <- as.numeric(difftime(t$beidz[6], t$sak[5], units = "days"))
    
    days <- sum(days1, days2, days3)
    rm(days1, days2, days3)
    
    yt <- y2[v, ]
    yt$dienas <- days
  } else if (t$zinkod[2] == "26" && 
             t$zinkod[3] == "81" && 
             t$zinkod[4] == "82" && 
             t$zinkod[5] == "11" && 
             t$zinkod[6] == "81" && 
             all(diff(t$NDZ_sanemsanas_datums[1:4]) == 0) &&
             all(diff(t$NDZ_sanemsanas_datums[5:6]) == 0) &&
             t$NDZ_sanemsanas_datums[4] != t$NDZ_sanemsanas_datums[5]) {
    yt <- y2[v, ]
    yt$dienas <- as.numeric(difftime(t$last_date[6], t$sak[5], units = "days")) + 1
  } else if (t$zinkod[2] == "53" && 
             t$zinkod[3] == "54" && 
             t$zinkod[4] == "53" && 
             t$zinkod[5] == "53" && 
             t$zinkod[6] == "54" && 
             all(diff(t$NDZ_sanemsanas_datums[1:2]) == 0) &&
             all(diff(t$NDZ_sanemsanas_datums[5:6]) == 0) &&
             all(!diff(t$NDZ_sanemsanas_datums[2:5]) == 0)) {

    days1 <- as.numeric(difftime(t$beidz[2], t$sak[1], units = "days")) + 1 
    days2 <- as.numeric(difftime(t$beidz[4], t$sak[3], units = "days"))
    days3 <- as.numeric(difftime(t$beidz[5], t$sak[6], units = "days"))
    days <- sum(days1, days2, days3)
    rm(days1, days2, days3)
    
    yt <- y2[v, ]
    yt$dienas <- days
  } else {
    stop("starpkodi6_11: Trūkst izstrādes koda starpkodi2.")
  }
  
  if(is.na(yt$PS_code[1])) {stop("Dienas NA.")}
  return(yt)
}

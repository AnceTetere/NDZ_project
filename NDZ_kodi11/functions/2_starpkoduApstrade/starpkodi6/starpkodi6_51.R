starpkodi6_51 <- function(y2, t, prev, v) {
  
  if (all(t$zinkod[c(1,2,4)] == "51") && all(t$zinkod[c(3,5)] == "50") && t$zinkod[6] == "25" && 
           all(diff(t$NDZ_sanemsanas_datums) != 0) && t$PS_code[1] == '___________' && t$NM_code[1] == '_______________') {
    yt <- y2[v, ]
    yt$dienas <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1, sapply(seq(2,5,by=2), function(i) as.numeric(difftime(t$beidz[i+1], t$sak[i], units = "days"))))
  } else if (t$zinkod[2] == "25" && t$zinkod[3] == "11" && t$zinkod[4] == "50" && 
      t$zinkod[5] == "25" && t$zinkod[6] == "51" && 
      all(diff(t$NDZ_sanemsanas_datums[1:5]) != 0) && all(diff(t$NDZ_sanemsanas_datums[5:6]) == 0)) {
    days1 <- as.numeric(difftime(t$beidz[2], t$sak[1], units = "days")) + 1 
    days2 <- as.numeric(difftime(t$beidz[4], t$sak[3], units = "days"))
    days3 <- 1 #atgriezās, lai tiktu atlaists - pieņemu, ka tā, bet varu kļūdīties.
    
    yt <- y2[v, ]
    yt$dienas <- sum(days1, days2, days3)
    rm(days1, days2, days3)
  } else if (all(t$zinkod[c(3,5)] == "51") && all(t$zinkod[c(2,4)] == "50") && t$zinkod[6] == "25" && 
             all(diff(t$NDZ_sanemsanas_datums) != 0)) {
    days1 <- as.numeric(difftime(t$beidz[2], t$sak[1], units = "days"))
    days2 <- as.numeric(difftime(t$beidz[4], t$sak[3], units = "days"))
    days3 <- as.numeric(difftime(t$beidz[6], t$sak[5], units = "days")) + 1 
    
    yt <- y2[v, ]
    yt$dienas <- sum(days1, days2, days3)
    rm(days1, days2, days3)
  } else if (all(t$zinkod[c(3,6)] == "51") && all(t$zinkod[c(2,4)] == "50") && t$zinkod[5] %in% c("21", "25") && 
             all(diff(t$NDZ_sanemsanas_datums[1:5]) != 0) && all(diff(t$NDZ_sanemsanas_datums[5:6]) == 0)) {
    days1 <- as.numeric(difftime(t$beidz[2], t$sak[1], units = "days"))
    days2 <- as.numeric(difftime(t$beidz[4], t$sak[3], units = "days"))
    
    yt <- y2[v, ]
    yt$dienas <- sum(days1, days2)
    rm(days1, days2)
  } else if (all(t$zinkod[c(3,5)] == "51") && all(t$zinkod[c(2,6)] == "50") && t$zinkod[4] == "21" && 
             all(diff(t$NDZ_sanemsanas_datums[1:4]) != 0) && all(diff(t$NDZ_sanemsanas_datums[4:6]) == 0)) {
    days1 <- as.numeric(difftime(t$beidz[2], t$sak[1], units = "days"))
    days2 <- as.numeric(difftime(t$beidz[4], t$sak[3], units = "days"))
    
    yt <- y2[v, ]
    yt$dienas <- sum(days1, days2)
    rm(days1, days2)
  } else if (all(t$zinkod[3:4] == "51") && t$zinkod[2] == "50" && t$zinkod[5] == "91" && t$zinkod[6] == "92" && 
             all(diff(t$NDZ_sanemsanas_datums) != 0)) {
    days1 <- as.numeric(difftime(t$beidz[2], t$sak[1], units = "days"))
    days2 <- as.numeric(difftime(t$beidz[5], t$sak[4], units = "days"))
    days3 <- as.numeric(difftime(t$last_date[6], t$sak[6], units = "days")) + 1
    
    yt <- y2[v, ]
    yt$dienas <- sum(days1, days2, days3)
    rm(days1, days2, days3)
  } else {
    stop("starpkodi6_51: Trūkst izstrādes koda starpkodi2.")
  }
  
  if(is.na(yt$PS_code[1])) {stop("Dienas NA.")}
  return(yt)
}

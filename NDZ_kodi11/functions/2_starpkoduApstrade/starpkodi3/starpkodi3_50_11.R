starpkodi3_50_11 <- function(y2, t, prev, v) {
  
  if (t$zinkod[3] == "51" && all(diff(t$NDZ_sanemsanas_datums[1:2]) != 0) && 
      all(diff(t$NDZ_sanemsanas_datums[2:3]) == 0)) {
    yt <- y2[v:(v+1), ]
    yt <- yt[yt$zinkod == "50", ]
  } else if (t$zinkod[3] == "51" && 
             all(diff(t$NDZ_sanemsanas_datums) != 0) &&
             t$PS_code[1] == '___________' && t$NM_code[1] == '___________') {
    days1 <- as.numeric(difftime(t$beidz[2], prev, units = "days")) - 1 
    days2 <- as.numeric(difftime(t$last_date[3], t$sak[3], units = "days")) + 1 
    
    yt <- y2[v, ]
    yt$dienas <- sum(days1, days2)
    rm(days1, days2)
  } else if (t$zinkod[3] == "51" && 
             all(diff(t$NDZ_sanemsanas_datums) != 0) &&
             t$PS_code[1] == '____________' && t$NM_code[1] == '___________') {
    yt <- y2[v:(v+1), ]
    yt <- yt[yt$zinkod == "50", ]
  } else {
    stop("starpkodi3_50_11: Iztrūkst apstrādes koda.")
  }
  
  
  if(is.na(yt$PS_code[1])) {stop("Dienas NA.")}
  return(yt) 
}

starpkodi6 <- function(y2, t, prev) {
  
  if (t$zinkod[1] == "11" && t$zinkod[2] == "25" && t$NDZ_sanemsanas_datums[1] <= t$NDZ_sanemsanas_datums[2] && t$zinkod[3] == "25" && t$zinkod[4] == "11" && t$zinkod[5] == "81" && t$zinkod[6] == "82" && all(t$NDZ_sanemsanas_datums[4:5] == t$NDZ_sanemsanas_datums[3])) {
    yt <- y2[v:(v+1), ]
    yt <- yt[yt$zinkod == "11", ]
  } else if (t$zinkod[1] == "11" && 
             t$zinkod[2] == "25" && 
             t$zinkod[3] == "11" && 
             t$zinkod[4] == "25" && 
             t$zinkod[5] == "81" && 
             t$zinkod[6] == "82" && 
             all(diff(t$NDZ_sanemsanas_datums[3:6]) == 0) &&
             all(!diff(t$NDZ_sanemsanas_datums[1:3]) == 0)) {
    
    days <- as.numeric(difftime(as.Date(t$beidz_darbu[2]), as.Date(t$sak_darbu[1]), units = "days")) + 1 # šis vieninieks ir tā viena diena no kodiem 81 un 82
    yt <- y2[v, ]
    yt$dienas <- days
  } else {
    stop("Starpkodi6 iztrūkst apstrādes koda.")
  }
  
  yt$zinkod <- "combined"  #jo starpkodu dienu sarēķins
  return(yt)
  rm(days, yt)
}

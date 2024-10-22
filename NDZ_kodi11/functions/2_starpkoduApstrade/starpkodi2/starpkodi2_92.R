starpkodi2_92 <- function(y, t, prev, v) {
  
  if (t$zinkod[2] == "21") {
    if(diff(t$NDZ_sanemsanas_datums) != 0) {
      yt <- y[v,]
      yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums)) + 1
    } else {stop("starpkodi2_92: Iztrūkst apstrādes koda")}
  } else {stop("starpkodi2_92: Iztrūkst apstrādes koda")}
  

  
  
  #if (t$zinkod[2] == "40") {
    #  yt <- y2[v,] 
    #  yt$dienas <- as.numeric(difftime(t$beidz_darbu[2], t$sak_darbu[1], units = "days"))
    #} else if (t$zinkod[2] == "25") {
    #  yt <- y2[v,] 
    #  yt$dienas <- as.numeric(difftime(t$beidz_darbu[2], t$sak_darbu[1], units = "days")) + 1 
    #} else  else if (t$zinkod[1] == "92" && t$zinkod[2] == "50") {
    #  yt <- y2[v,] 
    #  yt$dienas <- as.numeric(difftime(t$beidz_darbu[2], t$sak_darbu[1], units = "days"))
    #
  
 return(yt) 
}

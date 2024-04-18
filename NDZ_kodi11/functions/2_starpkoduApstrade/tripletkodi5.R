tripletkodi5 <- function(y3, t, prev, v) {
  
  if (t$zinkod[1] == "91" && t$zinkod[2] == "51" && 
      t$zinkod[3] == "92" && t$zinkod[4] == "50" && t$zinkod[5] == "21" && 
      t$NDZ_sanemsanas_datums[1] == t$NDZ_sanemsanas_datums[2] &&
      t$NDZ_sanemsanas_datums[2] != t$NDZ_sanemsanas_datums[3] &&
      t$NDZ_sanemsanas_datums[3] == t$NDZ_sanemsanas_datums[4] &&
      t$NDZ_sanemsanas_datums[4] != t$NDZ_sanemsanas_datums[5]) {

    yt <- y3[v, ]
    yt$dienas <- 0
  } else {
    stop("Tripletkodi5 iztrūkst apstrādes koda.")
  }
  
  if(is.na(yt$PS_code[1])) {stop("Dienas NA.")}
  yt$zinkod <- "combined"  #jo tripletkodu dienu sarēķins
  return(yt)
}

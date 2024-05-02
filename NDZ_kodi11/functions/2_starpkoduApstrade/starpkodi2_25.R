starpkodi2_25 <- function(y2, t, prev, v) {
  
  if(t$zinkod[2] == "40" && t$NDZ_sanemsanas_datums[1] == t$NDZ_sanemsanas_datums[2]) {
    yt <- y2[v:(v+1),] 
    yt <- yt[yt$zinkod == "40", ]
  } else if (t$zinkod[2] == "92" && t$NDZ_sanemsanas_datums[1] == t$NDZ_sanemsanas_datums[2]) {
    yt <- y2[v,] 
    yt$dienas <- 0
  } else if (t$zinkod[2] == "41" && t$NDZ_sanemsanas_datums[1] != t$NDZ_sanemsanas_datums[2]) {
    yt <- y2[v,] 
    yt$dienas <- 0
  } else if (t$zinkod[2] == "51" && t$NDZ_sanemsanas_datums[1] != t$NDZ_sanemsanas_datums[2]) {
    yt <- y2[v:(v+1),] 
    yt <- yt[yt$zinkod == "11", ]
  } else if (t$zinkod[2] == "50" && diff(t$NDZ_sanemsanas_datums[1:2]) != 0) {
    yt <- y2[v:(v+1),] 
    yt <- yt[yt$zinkod == "11", ]
  } else {
    stop("Starpkodi2: Trūkst izstrādes koda.")
  }
  
  if(is.na(yt$PS_code[1])) {stop("Dienas NA.")}
  yt$zinkod <- "combined"  #jo starpkodu dienu sarēķins
  return(yt)
}

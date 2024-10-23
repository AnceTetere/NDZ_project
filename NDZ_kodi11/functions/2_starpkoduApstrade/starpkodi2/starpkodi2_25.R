starpkodi2_25 <- function(y, t, prev, v) {
  
#  if(t$zinkod[2] == "40" && t$NDZ_sanemsanas_datums[1] == t$NDZ_sanemsanas_datums[2]) {
#    yt <- y[v:(v+1),] 
#    yt <- yt[yt$zinkod == "40", ]
#  } else if (t$zinkod[2] == "92" && t$NDZ_sanemsanas_datums[1] == t$NDZ_sanemsanas_datums[2]) {
#    yt <- y[v,] 
#    yt$dienas <- 0
#  } else 
  if (t$zinkod[2] %in% c("41", "51")) {
      if (diff(t$NDZ_sanemsanas_datums[1:2]) == 0) {
          yt <- y[v,]
          yt$dienas <- 0
        #} else if(diff(t$NDZ_sanemsanas_datums[1:2]) != 0) {
        #  yt <- y[v:(v+1),] 
        #  yt <- yt[yt$zinkod == "11", ]
      } else {stop("Starpkodi2_25: Trūkst izstrādes koda.")}
#  } else if (t$zinkod[2] == "50" && diff(t$NDZ_sanemsanas_datums[1:2]) != 0) {
#    yt <- y[v:(v+1),] 
#    yt <- yt[yt$zinkod == "11", ]
  } else {stop("Starpkodi2_25: Trūkst izstrādes koda.")}
  
  return(yt)
}

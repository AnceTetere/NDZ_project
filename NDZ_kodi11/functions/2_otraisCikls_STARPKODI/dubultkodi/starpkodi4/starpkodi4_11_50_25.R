starpkodi4_11_50_25 <- function(y, t, prev, v) {

if (t$zinkod[4] == "51") {
  if (all(diff(t$NDZ_sanemsanas_datums[1:3]) != 0) && diff(t$NDZ_sanemsanas_datums[3:4]) == 0) {
    yt <- y[v, ]
    yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[1:2])) 
  } else if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
    yt <- y[v, ]
    yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[1:2]))
  } else if (all(sapply(seq(1,4,by=2), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) && diff(t$NDZ_sanemsanas_datums[2:3]) != 0) {
    yt <- y[v, ]
    yt$dienas <- 0
  } else {stop("Starpkodi4_11_50_25: Trūkst izstrādes koda.")}
} else {stop("Starpkodi4_11_50_25: Trūkst izstrādes koda.")}
  
rm(y, t, prev, v)
return(yt) 
}

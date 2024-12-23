starpkodi5_11_50_25 <- function(y, t, prev, v){

  if (t$zk[4] %in% c("41", "51", "54", "92")) {
    if (t$zk[5] %in% c("41", "51", "54", "92")) {
      if (diff(t$NDZ_sanemsanas_datums[3:4]) == 0 && all(sapply(c(1,2,4), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
        yt <- y[v, ]
        yt$dd <- sum(sapply(c(1,3), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)])))
      } else {stop("Starpkodi5_11_50_25: Trūkst izstrādes koda.")}
    } else if (t$zk[5] %in% c("40", "50", "53", "91")) {
      if (diff(t$NDZ_sanemsanas_datums[2:3]) != 0 && all(sapply(c(1,3,4), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0))) {
        yt <- y[v, ]
        yt$dd <- sum(sapply(c(1,4), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)])))
      } else {stop("Starpkodi5_11_50_25: Trūkst izstrādes koda.")}
    } else {stop("Starpkodi5_11_50_25: Trūkst izstrādes koda.")}
    #} else if (t$zk[4] %in% c("40", "50", "53", "91")) {
    #        if (t$zk[5] %in% c("41", "51", "54", "92")) {
    #           if (diff(t$NDZ_sanemsanas_datums[2:3]) != 0 && all(sapply(c(1,3,4), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0))) {
    #             yt <- y[v, ]
    #             yt$dd <- 0
    #           } else {stop("Starpkodi5_11_50_25: Trūkst izstrādes koda.")}
    #        } else {stop("Starpkodi5_11_50_25: Trūkst izstrādes koda.")}
  } else {stop("Starpkodi5_11_50_25: Trūkst izstrādes koda.")}
  
  rm(y, t, prev, v)
  return(yt)
}

starpkodi6_25 <- function(y, t, prev, v) {
  
  if (t$zk[2] %in% c("11", "14", "16", "61")) {
    if (t$zk[3] %in% c("40", "50", "53", "91")) {
      if (t$zk[4] %in% c("41", "51", "54", "92")) {
        if (t$zk[5] %in% c("40", "50", "53", "91")) {
          if (t$zk[6] %in% c("41", "51", "54", "92")) {
            if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
              yt <- y[v, ]
              yt$dd <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")),
                               sapply(seq(2,5,by=2), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)])),
                               as.numeric(difftime(t$last_date[6], t$NDZ_sanemsanas_datums[6], units = "days")) + 1)
            } else {stop("starpkodi6_25: Trūkst izstrādes koda starpkodi2.")}
          } else {stop("starpkodi6_25: Trūkst izstrādes koda starpkodi2.")}
        } else {stop("starpkodi6_25: Trūkst izstrādes koda starpkodi2.")}
      } else {stop("starpkodi6_25: Trūkst izstrādes koda starpkodi2.")}
    } else {stop("starpkodi6_25: Trūkst izstrādes koda starpkodi2.")}
  } else {stop("starpkodi6_25: Trūkst izstrādes koda starpkodi2.")}
        
  rm(y, t, prev, v)
  return(yt)
}

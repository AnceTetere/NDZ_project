starpkodi4_51_50 <- function(y, t, prev, v) {
  
  if (t$zk[3] %in% c("21", "22", "23", "24", "25", "29")) {
    if (t$zk[4] %in% c("41", "51", "54", "92")) {
      if (all(diff(t$NDZ_sanemsanas_datums[1:3]) != 0) && diff(t$NDZ_sanemsanas_datums[3:4]) == 0) {
        yt <- y[v, ]
        yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[1:2]))
      } else if (all(saplly(c(1,3), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) && 
                 diff(t$NDZ_sanemsanas_datums[2:3]) != 0) {
        yt <- y[v, ]
        yt$dienas <- 0
      } else {stop("Starpkodi4_51: Trūkst izstrādes koda.")}
    }  else {stop("Starpkodi4_51: Trūkst izstrādes koda.")}
  } else if (t$zk[3] %in% c("41", "51", "54", "92")) {
    if (t$zk[4] %in% c("21", "22", "23", "24", "25", "29")) {
      if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
        yt <- y[v, ]
        yt$dienas <- sum(sapply(c(1,3), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]))) + 1
      } else {stop("Starpkodi4_51: Trūkst izstrādes koda.")}
    } else if (t$zk[4] %in% c("40", "50", "53", "91")) {
      if (diff(t$NDZ_sanemsanas_datums[1:2]) == 0 && all(diff(t$NDZ_sanemsanas_datums[2:4]) != 0)) {
        yt <- y[v, ]
        yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[3:4]))
      } else if (diff(t$NDZ_sanemsanas_datums[3:4]) == 0 && all(diff(t$NDZ_sanemsanas_datums[1:3]) != 0)) {
        yt <- y[v, ]
        yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[1:2]))
      } else {stop("Starpkodi4_51_50: Trūkst izstrādes koda.")}
    } else {stop("Starpkodi4_51_50: Trūkst izstrādes koda.")}
  
  rm(y, t, prev, v)
  return(yt)
}

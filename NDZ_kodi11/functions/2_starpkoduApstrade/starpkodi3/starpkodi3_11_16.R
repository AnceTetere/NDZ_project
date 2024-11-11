starpkodi3_11_16 <- function(y, t, prev, v) {
  
  if (t$zinkod[3] %in% c("40", "50", "53", "91")) {
    if (diff(t$NDZ_sanemsanas_datums[1:2]) == 0 && diff(t$NDZ_sanemsanas_datums[2:3]) != 0) {
      yt <- y[v,]
      yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[2:3]))
    } else if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
      yt <- y[v,]
      yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[2:3]))
    } else {stop("Starpkodi3_11_16: Trūkst izstrādes koda.")}
  } else {stop("Starpkodi3_11_16: Trūkst izstrādes koda.")}
  
  rm(y, t, prev, v); return(yt)
}

starpkodi4_11_50_11 <- function(y, t, prev, v) {
 
  if (t$zk[4] %in% c("41", "51", "54", "92")) {
    if (all(sapply(seq(1,4,by=2), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0))&&
        diff(t$NDZ_sanemsanas_datums[2:3]) != 0) {
      yt <- y[v, ]
      yt$dienas <- as.numeric(difftime(t$last_date[2], t$NDZ_sanemsanas_datums[4], units = "days")) + 1
    } else {stop("Starpkodi4_11_50_11: Trūkst izstrādes koda.")}
  } else {stop("Starpkodi4_11_50_11: Trūkst izstrādes koda.")}
  
  rm(y, t, prev, v)
  return(yt)
}

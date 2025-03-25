starpkodi5_11_50_25 <- function(y, t, prev, v){

  yt <- y[v, ]
  
  if (t$zinkod[4] %in% c("41", "51", "54", "92")) {
    if (t$zinkod[5] %in% c("41", "51", "54", "92")) {
      if (diff(t$NDZ_sanemsanas_datums[3:4]) == 0 && all(sapply(c(1,2,4), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
        yt$dienas <- sum(sapply(c(1,3), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)])))
      } else if (all(sapply(c(1,3), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) ||
                 all(sapply(c(2,4), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
        if (t$period[1] == "_____" && t$PS_code[1] == "_____" && t$NM_code[1] == "_____") {
          yt$dienas <- sum(sapply(c(1,3), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)])))
        } else {stop("Starpkodi5_11_50_25: Trūkst izstrādes koda.")}
      } else {stop("Starpkodi5_11_50_25: Trūkst izstrādes koda.")}
    } else if (t$zinkod[5] %in% c("40", "50", "53", "91")) {
      if (diff(t$NDZ_sanemsanas_datums[2:3]) != 0 && all(sapply(c(1,3,4), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0))) {
        yt$dienas <- sum(sapply(c(1,4), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)])))
      } else {stop("Starpkodi5_11_50_25: Trūkst izstrādes koda.")}
    } else {stop("Starpkodi5_11_50_25: Trūkst izstrādes koda.")}
    #} else if (t$zinkod[4] %in% c("40", "50", "53", "91")) {
    #        if (t$zinkod[5] %in% c("41", "51", "54", "92")) {
    #           if (diff(t$NDZ_sanemsanas_datums[2:3]) != 0 && all(sapply(c(1,3,4), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0))) {
    #             yt$dienas <- 0
    #           } else {stop("Starpkodi5_11_50_25: Trūkst izstrādes koda.")}
    #        } else {stop("Starpkodi5_11_50_25: Trūkst izstrādes koda.")}
  } else {stop("Starpkodi5_11_50_25: Trūkst izstrādes koda.")}
  
  rm(y, t, prev, v)
  return(yt)
}

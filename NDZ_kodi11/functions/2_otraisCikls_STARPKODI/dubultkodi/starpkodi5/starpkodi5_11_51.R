starpkodi5_11_51 <- function(y, t, prev, v) {
  
  yt <- y[v, ]
  
  if (t$zinkod[3] %in% c("40", "50", "53", "91")) {
    if (t$zinkod[4] %in% c("21", "22", "23", "24", "25", "29")) {
      if (t$zinkod[5] %in% c("41", "51", "54", "92")) {
       if (all(sapply(c(2,4), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
           all(sapply(c(1,3), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
           if (t$period[1] == '______' && t$PS_code[1] ==  '______________' &&  t$NM_code[1] ==  '______________') {
             yt$dienas <- sum(as.numeric(diff(t$NDZ_sanemsanas_datums[c(1,3)])),
                              as.numeric(diff(t$NDZ_sanemsanas_datums[4:5])))
           } else {stop("Starpkodi5_11_50: Trūkst izstrādes koda.")}
        } else {stop("Starpkodi5_11_50: Trūkst izstrādes koda.")}
      } else {stop("Starpkodi5_11_50: Trūkst izstrādes koda.")}
    } else {stop("Starpkodi5_11_50: Trūkst izstrādes koda.")}
  } else {stop("Starpkodi5_11_50: Trūkst izstrādes koda.")}
  
  rm(y, t, prev, v)
  return(yt)
}

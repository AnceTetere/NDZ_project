starpkodi6_11_50 <- function(y, t, prev, v) {
  
  yt <- y[v, ]
  
  if (t$zinkod[3] %in% c("41", "51", "54", "92")) {
    if (t$zinkod[4] %in% c("40", "50", "53", "91")) {
      if (t$zinkod[5] %in% c("21", "22", "23", "24", "25", "29")) {
        if (t$zinkod[6] %in% c("41", "51", "54", "92")) {
          if (all(sapply(seq(3, 6, by = 2), function(i) all(diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0))) && all(diff(t$NDZ_sanemsanas_datums[1:3]) != 0)) {
          yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[1:2]))
          } else if (diff(t$NDZ_sanemsanas_datums[5:6]) == 0 && all(diff(t$NDZ_sanemsanas_datums[1:5]) != 0)) {
            yt$dienas <- as.numeric(sum(diff(t$NDZ_sanemsanas_datums[1:2]), diff(t$NDZ_sanemsanas_datums[3:4])))
          } else if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
            yt$dienas <- as.numeric(sum(diff(t$NDZ_sanemsanas_datums[1:2]), diff(t$NDZ_sanemsanas_datums[3:4])))
          } else if (all(sapply(c(1,5), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
                     all(diff(t$NDZ_sanemsanas_datums[2:5]) != 0)) {
            yt$dienas <- sum(sapply(seq(1,6,by=2), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]))) 
          } else if (all(sapply(c(1,3,5), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) && 
                     all(sapply(c(2,4), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
            yt$dienas <- sum(sapply(c(1,3), function(i) diff(t$NDZ_sanemsanas_datums[1:2])))
          } else {stop("starpkodi6_11_50: Trūkst izstrādes koda starpkodi2.")}
        } else {stop("starpkodi6_11_50: Trūkst izstrādes koda starpkodi2.")}
      } else if (t$zinkod[5] %in% c("41", "51", "54", "92")) {
        if (t$zinkod[6] %in% c("40", "50", "53", "91")) {
          if (all(diff(t$NDZ_sanemsanas_datums[2:6]) != 0)) {
            yt$dienas <- sum(sapply(seq(1,6,by=2), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)])))
          } else {stop("starpkodi6_11_50: Trūkst izstrādes koda starpkodi2.")}
        } else if (all(t$zinkod[6] %in% c("21", "22", "23", "24", "25", "29"))) {
          if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
            yt$dienas <- sum(sapply(c(1,3,5), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)])))
          } else {stop("starpkodi6_11_50: Trūkst izstrādes koda starpkodi2.")}
        } else if (t$zinkod[6] %in% c("41", "51", "54", "92")) {
          if (all(diff(t$NDZ_sanemsanas_datums[1:2]) == 0) && all(diff(t$NDZ_sanemsanas_datums[5:6]) == 0) && all(diff(t$NDZ_sanemsanas_datums[2:5]) != 0)) {
            yt$dienas <- sum(sapply(seq(1, 6, by = 2), function(i) diff(t$NDZ_sanemsanas_datums[i:(i + 1)]))) + 1 
          } else {stop("starpkodi6_11_50: Trūkst izstrādes koda starpkodi2.")}  
        } else {stop("starpkodi6_11_50: Trūkst izstrādes koda starpkodi2.")}
    } else {stop("starpkodi6_11_50: Trūkst izstrādes koda starpkodi2.")}
  } else if (t$zinkod[4] %in% c("21", "22", "23", "24", "25", "29")) {
      if (t$zinkod[5] %in% c("41", "51", "54", "92")) {
        if (t$zinkod[6] %in% c("40", "50", "53", "91")) {
          if (all(diff(t$NDZ_sanemsanas_datums[1:4]) != 0) && all(diff(t$NDZ_sanemsanas_datums[4:6]) == 0)) {
            if ((t$period[1] == "_____" && t$PS_code[1] == "__________" && t$NM_code[1] == "__________") ||
                (t$period[1] == "_____" && t$PS_code[1] == "__________" && t$NM_code[1] == "__________")) {
              yt$dienas <- sum(sapply(c(1,3), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]))) + 1
            } else {stop("starpkodi6_11_50: Trūkst izstrādes koda starpkodi2.")}
          } else {stop("starpkodi6_11_50: Trūkst izstrādes koda starpkodi2.")}
        } else {stop("starpkodi6_11_50: Trūkst izstrādes koda starpkodi2.")}
      } else {stop("starpkodi6_11_50: Trūkst izstrādes koda starpkodi2.")}
    } else {stop("starpkodi6_11_50: Trūkst izstrādes koda starpkodi2.")}
} else {stop("starpkodi6_11_50: Trūkst izstrādes koda starpkodi2.")}

  rm(y, t, prev, v)
  return(yt)
}

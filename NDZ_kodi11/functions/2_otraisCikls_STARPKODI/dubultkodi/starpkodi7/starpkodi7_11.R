starpkodi7_11 <- function(y, t, prev, v) {
  
  yt <- y[v, ]
  
  if (t$zinkod[2] %in% c("40", "50", "53", "91")) {
    if (t$zinkod[3] %in% c("41", "51", "54", "92")) {
      if (t$zinkod[4] %in% c("40", "50", "53", "91")) {
        if (t$zinkod[5] %in% c("41", "51", "54", "92")) {
          if (t$zinkod[6] %in% c("40", "50", "53", "91")) {
            if (t$zinkod[7] %in% c("41", "51", "54", "92")) {
              if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
                yt$dienas <- sum(sapply(c(1,3,5), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)])),
                                 difftime(t$last_date[7], t$NDZ_sanemsanas_datums[7], units = "days")) + 1
              } else if (all(sapply(c(1:4,6), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
                yt$dienas <- sum(sapply(c(1,3,5), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)])),
                                 difftime(t$last_date[7], t$NDZ_sanemsanas_datums[7], units = "days")) + 1
              } else {stop("Starpkodi7_11: Trūkst izstrādes koda.")}
            } else if (t$zinkod[7] %in% c("21", "22", "23", "24", "25", "29")) {
              if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
                yt$dienas <- sum(sapply(c(1,3,5), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]))) 
              } else {stop("Starpkodi7_11: Trūkst izstrādes koda.")}
            } else {stop("Starpkodi7_11: Trūkst izstrādes koda.")}
          } else if (t$zinkod[6] %in% c("41", "51", "54", "92")) {
            if (t$zinkod[7] %in% c("21", "22", "23", "24", "25", "29")) {
              if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
                yt$dienas <- sum(sapply(c(1,3,6), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]))) + 1
              } else {stop("Starpkodi7_11: Trūkst izstrādes koda.")}
            } else if (t$zinkod[7] %in% c("21", "22", "23", "24", "25", "29")) {
              if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
                yt$dienas <- sum(sapply(c(1,3,5), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]))) 
              } else {stop("Starpkodi7_11: Trūkst izstrādes koda.")}
            } else {stop("Starpkodi7_11: Trūkst izstrādes koda.")}
          } else {stop("Starpkodi7_11: Trūkst izstrādes koda.")}
        } else {stop("Starpkodi7_11: Trūkst izstrādes koda.")}
      } else {stop("Starpkodi7_11: Trūkst izstrādes koda.")}
    } else {stop("Starpkodi7_11: Trūkst izstrādes koda.")}
  } else {stop("Starpkodi7_11: Trūkst izstrādes koda.")}

  rm(y, t, prev, v)
  return(yt) 
}

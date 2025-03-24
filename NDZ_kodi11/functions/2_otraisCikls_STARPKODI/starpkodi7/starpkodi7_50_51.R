starpkodi7_50_51 <- function(y, t, prev, v) {
  
  yt <- y[v, ]
  
  if (t$zinkod[3] %in% c("40", "50", "53", "91")) {
    if (t$zinkod[4] %in% c("41", "51", "54", "92")) {
      if (t$zinkod[5] %in% c("40", "50", "53", "91")) {
        if (t$zinkod[6] %in% c("21", "22", "23", "24", "25", "29")) {
          if (t$zinkod[7] %in% c("41", "51", "54", "92")) {
            if (all(diff(t$NDZ_sanemsanas_datums[1:6]) != 0) && diff(t$NDZ_sanemsanas_datums[6:7]) == 0) {
              if ((t$period[1] == "______" && t$pseidokods[1] == '______' && t$nmrkod[1] == '______') ||
                  (t$period[1] == "______" && t$pseidokods[1] == '______' && t$nmrkod[1] == '______')) {
                yt$dienas <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1,
                                 sapply(c(2,4), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)])),
                                 as.numeric(diff(t$NDZ_sanemsanas_datums[6:7])))
              } else {stop("starpkodi7_50_51 iztrūkst apstrādes koda.")}
            } else {stop("starpkodi7_50_51 iztrūkst apstrādes koda.")}
          } else {stop("starpkodi7_50_51 iztrūkst apstrādes koda.")}
        } else if (t$zinkod[6] %in% c("41", "51", "54", "92")) {
          if (t$zinkod[7] %in% c("21", "22", "23", "24", "25", "29")) {
            if (all(sapply(c(1,2,4,6), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0)) && 
                all(sapply(c(3,5), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0))) {
              if (t$period[1] == "_____" && t$pseidokods[1] == '______' && t$nmrkod[1] == '______') {
                yt$dienas <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1,
                                 sapply(c(2,4), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)])),
                                 as.numeric(diff(t$NDZ_sanemsanas_datums[6:7]))) + 1
              } else {stop("starpkodi7_50_51 iztrūkst apstrādes koda.")}
            } else if (diff(t$NDZ_sanemsanas_datums[1:2]) == 0 && all(diff(t$NDZ_sanemsanas_datums[2:7]) != 0)) {
              if ((t$period[1] == "_____" && t$pseidokods[1] == '______' && t$nmrkod[1] == '______') ||
                  (t$period[1] == "_____" && t$pseidokods[1] == '______' && t$nmrkod[1] == '______')) {
                yt$dienas <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1,
                                 sapply(c(2,4,6), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]))) + 1
              } else {stop("starpkodi7_50_51 iztrūkst apstrādes koda.")}
            } else if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
              if (t$period[1] == "______" && t$pseidokods[1] == '______' && t$nmrkod[1] == '______') {
                yt$dienas <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1,
                                 sapply(c(2,4,6), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]))) + 1
              } else {stop("starpkodi7_50_51 iztrūkst apstrādes koda.")}
            } else {stop("starpkodi7_50_51 iztrūkst apstrādes koda.")}
          } else {stop("starpkodi7_50_51 iztrūkst apstrādes koda.")}
        } else {stop("starpkodi7_50_51 iztrūkst apstrādes koda.")}
      } else {stop("starpkodi7_50_51 iztrūkst apstrādes koda.")}
    } else {stop("starpkodi7_50_51 iztrūkst apstrādes koda.")}
  #} else if (t$zinkod[3] %in% c("41", "51", "54", "92")) {
  #  if (t$zinkod[4] %in% c("41", "51", "54", "92")) {
  #    if (t$zinkod[5] %in% c("40", "50", "53", "91")) {
  #      if (t$zinkod[6] %in% c("41", "51", "54", "92")) {
  #        if (t$zinkod[7] %in% c("40", "50", "53", "91")) {
  #          if (all(sapply(c(1,3,4,5,6), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0)) && 
  #              diff(t$NDZ_sanemsanas_datums[2:3]) == 0) {
  #            yt <- y[v, ]
  #            yt$dienas <- sum(sapply(c(1,4,6), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]))) + 1
  #          } else {stop("starpkodi7_50_51 iztrūkst apstrādes koda.")}
  #        } else {stop("starpkodi7_50_51 iztrūkst apstrādes koda.")}
  #      } else {stop("starpkodi7_50_51 iztrūkst apstrādes koda.")}
  #    } else {stop("starpkodi7_50_51 iztrūkst apstrādes koda.")}
  #  } else if (t$zinkod[4] %in% c("40", "50", "53", "91")) {
  #    if (t$zinkod[5] %in% c("41", "51", "54", "92")) {
  #      if (t$zinkod[6] %in% c("40", "50", "53", "91")) {
  #        if (t$zinkod[7] %in% c("21", "22", "23", "25", "24", "29")) {
  #          if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
  #            yt <- y[v, ]
  #            yt$dienas <- sum(sapply(seq(1,6,by=2), function(i) as.numeric(diff(t$NDZ_sanemsanas_datums[i:(i+1)]))))
  #          } else {stop("starpkodi7_50_51 iztrūkst apstrādes koda.")}
  #        } else {stop("starpkodi7_50_51 iztrūkst apstrādes koda.")}
  #      } else {stop("starpkodi7_50_51 iztrūkst apstrādes koda.")}
  #    } else {stop("starpkodi7_50_51 iztrūkst apstrādes koda.")}
  #  } else {stop("starpkodi7_50_51 iztrūkst apstrādes koda.")}
  } else {stop("starpkodi7_50_51 iztrūkst apstrādes koda.")}
  
  rm(y, t, prev, v)
  return(yt)
}

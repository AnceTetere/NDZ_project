starpkodi7_51_50 <- function(y, t, prev, v) {
  
  yt <- y[v, ]
  
    if (t$zinkod[3] %in% c("40", "50", "53", "91")) {
      if (t$zinkod[4] %in% c("41", "51", "54", "92")) {
        if (t$zinkod[5] %in% c("21", "22", "23", "25", "24", "29")) {
          if (t$zinkod[6] %in% c("41", "51", "54", "92")) {
            if (t$zinkod[7] %in% c("41", "51", "54", "92")) {
              if (all(sapply(c(1,2,4,5,6), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0)) && 
                  diff(t$NDZ_sanemsanas_datums[3:4]) == 0) {
                if (t$PS_code[1] ==  '______________' &&  t$NM_code[1] ==  '______________') {
                  yt$dienas <- as.numeric(sum(diff(t$NDZ_sanemsanas_datums[1:2])),
                                          diff(t$NDZ_sanemsanas_datums[c(3,5)]) + 1)
                } else {stop("starpkodi7_51_50 iztrūkst apstrādes koda.")}
              } else {stop("starpkodi7_51_50 iztrūkst apstrādes koda.")}
            } else {stop("starpkodi7_51_50 iztrūkst apstrādes koda.")}
          } else {stop("starpkodi7_51_50 iztrūkst apstrādes koda.")}
        } else {stop("starpkodi7_51_50 iztrūkst apstrādes koda.")}
      } else {stop("starpkodi7_51_50 iztrūkst apstrādes koda.")}
    } else if (t$zinkod[3] %in% c("41", "51", "54", "92")) {
      if (t$zinkod[4] %in% c("41", "51", "54", "92")) {
        if (t$zinkod[5] %in% c("40", "50", "53", "91")) {
          if (t$zinkod[6] %in% c("41", "51", "54", "92")) {
            if (t$zinkod[7] %in% c("40", "50", "53", "91")) {
              if (all(sapply(c(1,3,4,5,6), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0)) && 
                  diff(t$NDZ_sanemsanas_datums[2:3]) == 0) {
                yt$dienas <- sum(sapply(c(1,4,6), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]))) + 1
              } else {stop("starpkodi7_51_50 iztrūkst apstrādes koda.")}
            } else {stop("starpkodi7_51_50 iztrūkst apstrādes koda.")}
          } else if (t$zinkod[6] %in% c("21", "22", "23", "25", "24", "29")) {
            if (t$zinkod[7] %in% c("41", "51", "54", "92")) {
              if (all(diff(t$NDZ_sanemsanas_datums[1:6]) != 0) && 
                  diff(t$NDZ_sanemsanas_datums[6:7]) == 0) {
                yt$dienas <- sum(sapply(c(1,4), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)])),
                                 as.numeric(diff(t$NDZ_sanemsanas_datums[6:7])))
              } else {stop("starpkodi7_51_50 iztrūkst apstrādes koda.")}
            } else {stop("starpkodi7_51_50 iztrūkst apstrādes koda.")}
          } else {stop("starpkodi7_51_50 iztrūkst apstrādes koda.")}
        } else {stop("starpkodi7_51_50 iztrūkst apstrādes koda.")}
      } else if (t$zinkod[4] %in% c("40", "50", "53", "91")) {
        if (t$zinkod[5] %in% c("41", "51", "54", "92")) {
          if (t$zinkod[6] %in% c("40", "50", "53", "91")) {
            if (t$zinkod[7] %in% c("21", "22", "23", "25", "24", "29")) {
              if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
                yt$dienas <- sum(sapply(seq(1,6,by=2), function(i) as.numeric(diff(t$NDZ_sanemsanas_datums[i:(i+1)]))))
              } else {stop("starpkodi7_51_50 iztrūkst apstrādes koda.")}
            } else if (t$zinkod[7] %in% c("41", "51", "54", "92")) {
              if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
                if (t$period[1] == '______' && t$PS_code[1] ==  '______________' &&  t$NM_code[1] ==  '______________') {
                  yt$dienas <- sum(sapply(seq(1,6,by=2), function(i) as.numeric(diff(t$NDZ_sanemsanas_datums[i:(i+1)]))),
                                   as.numeric(difftime(t$last_date[7], t$NDZ_sanemsanas_datums[7], units = 'days')))
                } else {stop("starpkodi7_51_50 iztrūkst apstrādes koda.")}
              } else {stop("starpkodi7_51_50 iztrūkst apstrādes koda.")}
            } else {stop("starpkodi7_51_50 iztrūkst apstrādes koda.")}
          } else {stop("starpkodi7_51_50 iztrūkst apstrādes koda.")}
        } else {stop("starpkodi7_51_50 iztrūkst apstrādes koda.")}
      } else {stop("starpkodi7_51_50 iztrūkst apstrādes koda.")}
    } else if (t$zinkod[3] %in% c("21", "22", "23", "25", "24", "29")) {
      if (t$zinkod[4] %in% c("41", "51", "54", "92")) {
        if (t$zinkod[5] %in% c("41", "51", "54", "92")) {
          if (t$zinkod[6] %in% c("21", "22", "23", "25", "24", "29")) {
            if (t$zinkod[7] %in% c("11", "14", "16", "61")) {
              if (all(sapply(c(3,6), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
                  all(sapply(c(1,2,4,5), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
                  if (t$period[1] == '______' && t$PS_code[1] ==  '______________' &&  t$NM_code[1] ==  '______________') {
                  yt$dienas <- sum(sapply(c(1,3,6), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]))) + 1
                  } else {stop("starpkodi7_51_50 iztrūkst apstrādes koda.")}
              } else {stop("starpkodi7_51_50 iztrūkst apstrādes koda.")}
            } else {stop("starpkodi7_51_50 iztrūkst apstrādes koda.")}
          } else {stop("starpkodi7_51_50 iztrūkst apstrādes koda.")}
        } else {stop("starpkodi7_51_50 iztrūkst apstrādes koda.")}
      } else {stop("starpkodi7_51_50 iztrūkst apstrādes koda.")}
    } else{stop("starpkodi7_51_50 iztrūkst apstrādes koda.")}
    
  rm(y, t, prev, v)
  return(yt)
}

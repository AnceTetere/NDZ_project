starpkodi6_50_51 <- function(y, t, prev, v) {
  
  yt <- y[v, ]
  
    if (t$zinkod[3] %in% c("40", "50", "53", "91")) {
      if (t$zinkod[4] %in% c("41", "51", "54", "92")) {
        if (t$zinkod[5] %in% c("21", "22", "23", "24", "25", "29")) {
          if (t$zinkod[6] %in% c("41", "51", "54", "92")) {
            if (diff(t$NDZ_sanemsanas_datums[5:6]) == 0 && all(diff(t$NDZ_sanemsanas_datums[1:5]) != 0)) {
              yt$dienas <- as.numeric(sum(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days") - 1, 
                                          diff(t$NDZ_sanemsanas_datums[2:3]),
                                          diff(t$NDZ_sanemsanas_datums[4:5]),
                                          difftime(t$last_date[6], t$NDZ_sanemsanas_datums[6], units = "days") + 1))
            } else if (all(sapply(c(1,3,5), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
                       all(sapply(c(2,4), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
              if (t$period[1] == '______' && t$PS_code[1] ==  '______________' &&  t$NM_code[1] ==  '______________') {
                yt$dienas <- sum(sapply(c(1,3,5), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)])))
              } else {stop("starpkodi6_91: Trūkst izstrādes koda.")}
            } else if (all(sapply(c(1,5), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
                       all(diff(t$NDZ_sanemsanas_datums[2:5]) != 0)) {
              if (t$period[1] == '______' && t$PS_code[1] ==  '______________' &&  t$NM_code[1] ==  '______________') {
                yt$dienas <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")),
                                 sapply(c(2,5), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)])))
              } else {stop("starpkodi6_91: Trūkst izstrādes koda.")}
            } else if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
              if (t$period[1] == '______' && t$PS_code[1] ==  '______________' &&  t$NM_code[1] ==  '______________') {
                yt$dienas <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1,
                                 sapply(c(2,4), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]))) + 1
              } else {stop("starpkodi6_91: Trūkst izstrādes koda.")}
            } else {stop("starpkodi6_91: Trūkst izstrādes koda.")}
          } else if (t$zinkod[6] %in% c("11", "14", "16", "61")) {
            if (all(t$NDZ_sanemsanas_datums != 0)) {              
              if (t$period[1] == '______' && t$PS_code[1] ==  '______________' && t$NM_code[1] ==  '______________') {
                yt$dienas <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")),
                                 sapply(c(2,4), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)])),
                                 as.numeric(difftime(t$last_date[6], t$NDZ_sanemsanas_datums[6], units = "days"))) + 1
              } else {stop("starpkodi6_91: Trūkst izstrādes koda.")}
            } else {stop("starpkodi6_91: Trūkst izstrādes koda.")}
          } else {stop("starpkodi6_91: Trūkst izstrādes koda.")}
        } else if (t$zinkod[5] %in% c("40", "50", "53", "91")) {
          if (t$zinkod[6] %in% c("21", "22", "23", "24", "25", "29")) {
            if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
              yt$dienas <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1, 
                               sapply(c(2,4), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)])))
            } else {stop("starpkodi6_91: Trūkst izstrādes koda.")}
          } else if (t$zinkod[6] %in% c("41", "51", "54", "92")) {
            if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
              yt$dienas <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1, 
                               sapply(c(2,4), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)])),
                               as.numeric(difftime(t$last_date[6], t$NDZ_sanemsanas_datums[6], units = "days")))
            } else {stop("starpkodi6_91: Trūkst izstrādes koda.")}
          } else {stop("starpkodi6_91: Trūkst izstrādes koda.")}
        } else if (t$zinkod[5] %in% c("41", "51", "54", "92")) {
                if (t$zinkod[6] %in% c("40", "50", "53", "91")) {
                  if (all(sapply(c(1,3), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
                      all(sapply(c(2,4,5), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
                      if (t$period[1] == '______' && t$PS_code[1] ==  '______________' &&  t$NM_code[1] ==  '______________') {
                        t <- t[c(4,3,5,6),]
                        yt$dienas <- sum(sapply(c(1,3), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)])))
                      } else {stop("starpkodi6_91: Trūkst izstrādes koda.")}
                  } else {stop("starpkodi6_91: Trūkst izstrādes koda.")}
                } else {stop("starpkodi6_91: Trūkst izstrādes koda.")}
        } else {stop("starpkodi6_91: Trūkst izstrādes koda.")}
      #} else if (t$zinkod[4] %in% c("40", "50", "53", "91")) {
      #    if (t$zinkod[5] %in% c("21", "22", "23", "24", "25", "29")) {
      #     if (t$zinkod[6] %in% c("41", "51", "54", "92")) {
      #       if (t$period[1] == '______' && t$PS_code[1] ==  '______________' &&  t$NM_code[1] ==  '______________') {
      #         
      #         yt$dienas <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1,
      #                          as.numeric(diff(t$NDZ_sanemsanas_datums[c(2,4)])),
      #                          as.numeric(diff(t$NDZ_sanemsanas_datums[5:6])))  #Pieņemu, ka atlaišana tika paziņota
            # } else {stop("starpkodi6_91: Trūkst izstrādes koda.")}
        } else {stop("starpkodi6_91: Trūkst izstrādes koda.")}
    } else if (t$zinkod[3] %in% c("21", "22", "23", "24", "25", "29")) {
            if (t$zinkod[4] %in% c("11", "14", "16", "61")) {
              if (t$zinkod[5] %in% c("41", "51", "54", "92")) {
                if (t$zinkod[6] %in% c("11", "14", "16", "61")) {
                  if (all(sapply(c(1,2,5), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0)) &&
                      all(diff(t$NDZ_sanemsanas_datums[3:5]) == 0)) {
                      if (t$period[1] == '______' && t$PS_code[1] ==  '______________' &&  t$NM_code[1] ==  '______________') {
                        yt$dienas <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1, 
                                         diff(t$NDZ_sanemsanas_datums[c(3,5)]),
                                         as.numeric(difftime(t$last_date[6], t$NDZ_sanemsanas_datums[6], units = "days"))) + 1
                      } else {stop("starpkodi6_91: Trūkst izstrādes koda.")}
                  } else {stop("starpkodi6_91: Trūkst izstrādes koda.")}
                } else {stop("starpkodi6_91: Trūkst izstrādes koda.")}
              } else {stop("starpkodi6_91: Trūkst izstrādes koda.")}
            } else {stop("starpkodi6_91: Trūkst izstrādes koda.")}
      } else {stop("starpkodi6_91: Trūkst izstrādes koda.")}
  
  rm(y, t, prev, v)
  return(yt)
}

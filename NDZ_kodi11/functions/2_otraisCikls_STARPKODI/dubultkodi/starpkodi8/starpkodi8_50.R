 starpkodi8_50 <- function(y, t, prev, v) {
  
  yt <- y[v, ]

  if (t$zinkod[2] %in% c("41", "51", "54", "92")) {
    yt <- starpkodi8_50_51(y, t, prev, v)
  } else if (t$zinkod[2] %in% c("40", "50", "53", "91")) {
           if (t$zinkod[3] %in% c("41", "51", "54", "92")) {
             if (t$zinkod[4] %in% c("41", "51", "54", "92")) {
               if (t$zinkod[5] %in% c("40", "50", "53", "91")) {
                 if (t$zinkod[6] %in% c("41", "51", "54", "92")) {
                if (t$zinkod[7] %in% c("40", "50", "53", "91")) {
                  if (t$zinkod[8] %in% c("41", "51", "54", "92")) {
                    if (all(sapply(c(1,3), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
                        all(sapply(c(2,4,5,6,7), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
                        if (t$period[1] == '______' && t$PS_code[1] ==  '______________' && t$NM_code[1] ==  '______________'){
                          yt$dienas <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1,
                                           as.numeric(diff(t$NDZ_sanemsanas_datums[c(3,5)])),
                                           as.numeric(diff(t$NDZ_sanemsanas_datums[6:7])),
                                           as.numeric(difftime(t$last_date[8], t$NDZ_sanemsanas_datums[8], units = "days"))) + 1
                        } else {stop("starpkodi8_50 iztrūkst apstrādes koda.")}
                      } else {stop("starpkodi8_50 iztrūkst apstrādes koda.")} 
                    } else {stop("starpkodi8_50 iztrūkst apstrādes koda.")}
                  } else {stop("starpkodi8_50 iztrūkst apstrādes koda.")}
                 } else if (t$zinkod[6] %in% c("40", "50", "53", "91")) {
                          if (t$zinkod[7] %in% c("41", "51", "54", "92")) {
                            if (t$zinkod[8] %in% c("41", "51", "54", "92")) {
                              if (all(sapply(seq(1,8,by=2), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
                                  all(sapply(seq(2,7,by=2), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
                                  if ((t$period[1] == '______' && t$PS_code[1] ==  '______________' && t$NM_code[1] ==  '______________') ||
                                      (t$period[1] == '______' && t$PS_code[1] ==  '______________' && t$NM_code[1] ==  '______________')) {
                                      yt$dienas <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1,
                                                       as.numeric(diff(t$NDZ_sanemsanas_datums[c(3,5)])),
                                                       as.numeric(difftime(t$last_date[8], t$NDZ_sanemsanas_datums[8], units = "days"))) + 1
                                 } else {stop("starpkodi8_50 iztrūkst apstrādes koda.")}
                              } else {stop("starpkodi8_50 iztrūkst apstrādes koda.")} 
                          } else {stop("starpkodi8_50 iztrūkst apstrādes koda.")}
                      } else {stop("starpkodi8_50 iztrūkst apstrādes koda.")}
                  } else {stop("starpkodi8_50 iztrūkst apstrādes koda.")}
              } else {stop("starpkodi8_50 iztrūkst apstrādes koda.")}
          } else {stop("starpkodi8_50 iztrūkst apstrādes koda.")}
      } else if (t$zinkod[3] %in% c("40", "50", "53", "91")) {
               if (t$zinkod[4] %in% c("41", "51", "54", "92")) {
                 if (t$zinkod[5] %in% c("40", "50", "53", "91")) {
                   if (t$zinkod[6] %in% c("41", "51", "54", "92")) {
                     if (t$zinkod[7] %in% c("40", "50", "53", "91")) {
                       if (t$zinkod[8] %in% c("41", "51", "54", "92")) {
                         if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
                             yt$dienas <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1,
                                              sapply(c(2,4,6), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)])),
                                              as.numeric(difftime(t$last_date[8], t$NDZ_sanemsanas_datums[8], units = "days")))
                        } else {stop("starpkodi8_50 iztrūkst apstrādes koda.")} 
                    } else if (t$zinkod[8] %in% c("21", "22", "23", "24", "25", "29")) {
                             if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
                                 yt$dienas <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1,
                                                  sapply(c(2,4,6), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)])))
                             } else {stop("starpkodi8_50 iztrūkst apstrādes koda.")} 
                    } else {stop("starpkodi8_50 iztrūkst apstrādes koda.")}
                } else {stop("starpkodi8_50 iztrūkst apstrādes koda.")}
            } else {stop("starpkodi8_50 iztrūkst apstrādes koda.")}
        } else {stop("starpkodi8_50 iztrūkst apstrādes koda.")}
    } else {stop("starpkodi8_50 iztrūkst apstrādes koda.")}
} else {stop("starpkodi8_50 iztrūkst apstrādes koda.")}
} else {stop("starpkodi8_50 iztrūkst apstrādes koda.")}  

  rm(y, t, prev, v)
  return(yt)
}

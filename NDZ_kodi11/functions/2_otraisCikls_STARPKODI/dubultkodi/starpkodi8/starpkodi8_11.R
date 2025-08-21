starpkodi8_11 <- function(y, t, prev, v) {
  
  yt <- y[v,]
 
  if (t$zinkod[2] %in% c("40", "50", "53", "91")) {
    if (t$zinkod[3] %in% c("41", "51", "54", "92")) {
      if (t$zinkod[4] %in% c("40", "50", "53", "91")) {
        if (t$zinkod[5] %in% c("41", "51", "54", "92")) {
          if (t$zinkod[6] %in% c("40", "50", "53", "91")) {
            if (t$zinkod[7] %in% c("21", "22", "23", "24", "25", "29")) {
              if (t$zinkod[8] %in% c("41", "51", "54", "92")) {
                if (diff(t$NDZ_sanemsanas_datums[7:8]) == 0 && all(diff(t$NDZ_sanemsanas_datums[1:7]) != 0)) {
                  if ((t$period[1] == '______' && t$PS_code[1] ==  '______________' && t$NM_code[1] ==  '______________') ||
                      (t$period[1] == '______' && t$PS_code[1] ==  '______________' && t$NM_code[1] ==  '______________')) {
                        yt$dienas <- sum(sapply(seq(1,7,by=2), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)])))                  
                    } else {stop("Starpkodi8_11 iztrūkst apstrādes koda.")}
                } else {stop("Starpkodi8_11 iztrūkst apstrādes koda.")}
              } else {stop("Starpkodi8_11 iztrūkst apstrādes koda.")}
            } else if (t$zinkod[7] %in% c("41", "51", "54", "92")) {
              if (t$zinkod[8] %in% c("40", "50", "53", "91")) {
                if (diff(t$NDZ_sanemsanas_datums[1:2]) == 0 && all(diff(t$NDZ_sanemsanas_datums[2:8]) != 0)) {
                  if (t$period[1] == '______' && t$PS_code[1] ==  '______________' && t$NM_code[1] ==  '______________') {
                    yt$dienas <- sum(sapply(seq(1,7,by=2), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)])))                  
                  } else {stop("Starpkodi8_11 iztrūkst apstrādes koda.")}
                } else {stop("Starpkodi8_11 iztrūkst apstrādes koda.")}
              } else if (t$zinkod[8] %in% c("21", "22", "23", "24", "25", "29")) {
                if (diff(t$NDZ_sanemsanas_datums[1:2]) == 0 && all(diff(t$NDZ_sanemsanas_datums[2:8]) != 0)) {
                  if (t$period[1] == '______' && t$PS_code[1] ==  '______________' &&  t$NM_code[1] ==  '______________') {
                    yt$dienas <- sum(sapply(seq(1,7,by=2), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]))) + 1              
                  } else {stop("Starpkodi8_11 iztrūkst apstrādes koda.")}
                } else if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
                  if (t$period[1] == '______' && t$PS_code[1] ==  '______________' &&  t$NM_code[1] ==  '______________') {
                    yt$dienas <- sum(sapply(seq(1,7,by=2), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]))) + 1              
                  } else {stop("Starpkodi8_11 iztrūkst apstrādes koda.")}
                } else {stop("Starpkodi8_11 iztrūkst apstrādes koda.")}
              } else {stop("Starpkodi8_11 iztrūkst apstrādes koda.")}
            } else {stop("Starpkodi8_11 iztrūkst apstrādes koda.")}
          } else {stop("Starpkodi8_11 iztrūkst apstrādes koda.")}
        } else {stop("Starpkodi8_11 iztrūkst apstrādes koda.")}
      } else {stop("Starpkodi8_11 iztrūkst apstrādes koda.")}
    } else {stop("Starpkodi8_11 iztrūkst apstrādes koda.")}
  } else {stop("Starpkodi8_11 iztrūkst apstrādes koda.")} 

  rm(y, t, prev, v)
  return(yt)
}

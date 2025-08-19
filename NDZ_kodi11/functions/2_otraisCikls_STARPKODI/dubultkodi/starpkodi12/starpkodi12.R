starpkodi12 <- function(y, t, prev, v) {
  
  yt <- y[v, ]
  
  if (t$zinkod[1] %in% c("40", "50", "53", "91")) {
    if (t$zinkod[2] %in% c("40", "50", "53", "91")) {
      if (t$zinkod[3] %in% c("41", "51", "54", "92")) {
        if (t$zinkod[4] %in% c("41", "51", "54", "92")) {          
          if (t$zinkod[5] %in% c("40", "50", "53", "91")) {
            if (t$zinkod[6] %in% c("40", "50", "53", "91")) {
              if (t$zinkod[7] %in% c("41", "51", "54", "92")) {
                if (t$zinkod[8] %in% c("41", "51", "54", "92")) {
                  if (t$zinkod[9] %in% c("40", "50", "53", "91")) {
                    if (t$zinkod[10] %in% c("40", "50", "53", "91")) {
                      if (t$zinkod[11] %in% c("41", "51", "54", "92")) {
                        if (t$zinkod[12] %in% c("41", "51", "54", "92")) {
                          if (all(sapply(seq(1,11,by=2), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
                              all(sapply(seq(2,11,by=2), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {                              
                              if (t$period[1] == '______' && t$PS_code[1] ==  '______________' &&  t$NM_code[1] ==  '______________') {
                                  yt$dienas <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")),
                                                   sapply(c(3,7), function(i) diff(t$NDZ_sanemsanas_datums[c(i,(i+2))])),
                                                   as.numeric(difftime(t$last_date[12], t$NDZ_sanemsanas_datums[12], units = "days")) + 1)
                            } else {stop("Starpkodi12: iztrūkst apstrādes koda.")}
                          } else {stop("Starpkodi12: iztrūkst apstrādes koda.")}
                        } else {stop("Starpkodi12: iztrūkst apstrādes koda.")}
                      } else {stop("Starpkodi12: iztrūkst apstrādes koda.")}
                    } else {stop("Starpkodi12: iztrūkst apstrādes koda.")}
                  } else {stop("Starpkodi12: iztrūkst apstrādes koda.")}
                } else {stop("Starpkodi12: iztrūkst apstrādes koda.")}
              } else {stop("Starpkodi12: iztrūkst apstrādes koda.")}
            } else {stop("Starpkodi12: iztrūkst apstrādes koda.")}
          } else {stop("Starpkodi12: iztrūkst apstrādes koda.")}
        } else {stop("Starpkodi12: iztrūkst apstrādes koda.")}
      } else {stop("Starpkodi12: iztrūkst apstrādes koda.")}
    } else {stop("Starpkodi12: iztrūkst apstrādes koda.")}
  } else {stop("Starpkodi12: iztrūkst apstrādes koda.")}

  if(is.na(yt$PS_code[1])) {stop("Dienas NA.")}
  yt$zinkod <- "combined"  #jo starpkodu dienu sarēķins
  return(yt)
} 

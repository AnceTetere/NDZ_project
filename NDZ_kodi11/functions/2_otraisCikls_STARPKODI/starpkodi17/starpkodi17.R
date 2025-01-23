starpkodi17 <- function(y, t, prev, v) {
  
  yt <- y[v, ] 
  
  if (t$zinkod[1] %in% c("40", "50", "53", "91")) {
    if (t$zinkod[2] %in% c("41", "51", "54", "92")) {
      if (t$zinkod[3] %in% c("40", "50", "53", "91")) {
        if (t$zinkod[4] %in% c("41", "51", "54", "92")) {
          if (t$zinkod[5] %in% c("40", "50", "53", "91")) {
            if (t$zinkod[6] %in% c("41", "51", "54", "92")) {
              if (t$zinkod[7] %in% c("40", "50", "53", "91")) {
                if (t$zinkod[8] %in% c("41", "51", "54", "92")) {
                  if (t$zinkod[9] %in% c("40", "50", "53", "91")) {
                    if (t$zinkod[10] %in% c("41", "51", "54", "92")) {
                      if (t$zinkod[11] %in% c("40", "50", "53", "91")) {
                        if (t$zinkod[12] %in% c("41", "51", "54", "92")) {
                          if (t$zinkod[13] %in% c("40", "50", "53", "91")) {
                            if (t$zinkod[14] %in% c("41", "51", "54", "92")) {
                              if (t$zinkod[15] %in% c("40", "50", "53", "91")) {
                                if (t$zinkod[16] %in% c("21", "22", "23", "24", "25", "29")) {
                                  if (t$zinkod[17] %in% c("41", "51", "54", "92")) {
                                    if (all(diff(t$NDZ_sanemsanas_datums[1:16]) != 0) && diff(t$NDZ_sanemsanas_datums[16:17]) == 0) {
                                      yt$dienas <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")),
                                                       sapply(seq(2,14,by=2), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)])),
                                                       as.numeric(diff(t$NDZ_sanemsanas_datums[16:17])))
                                    } else {stop("starpkodi17 iztrūkst apstrādes koda.")}
                                  } else {stop("starpkodi17 iztrūkst apstrādes koda.")}
                                } else {stop("starpkodi17 iztrūkst apstrādes koda.")}
                              } else {stop("starpkodi17 iztrūkst apstrādes koda.")}
                            } else {stop("starpkodi17 iztrūkst apstrādes koda.")}
                          } else {stop("starpkodi17 iztrūkst apstrādes koda.")}
                        } else {stop("starpkodi17 iztrūkst apstrādes koda.")}
                      } else {stop("starpkodi17 iztrūkst apstrādes koda.")}
                    } else {stop("starpkodi17 iztrūkst apstrādes koda.")}
                  } else {stop("starpkodi17 iztrūkst apstrādes koda.")}
                } else {stop("starpkodi17 iztrūkst apstrādes koda.")}
              } else {stop("starpkodi17 iztrūkst apstrādes koda.")}
            } else {stop("starpkodi17 iztrūkst apstrādes koda.")}
          } else {stop("starpkodi17 iztrūkst apstrādes koda.")}
        } else {stop("starpkodi17 iztrūkst apstrādes koda.")}
      } else {stop("starpkodi17 iztrūkst apstrādes koda.")}
    } else {stop("starpkodi17 iztrūkst apstrādes koda.")}
  } else {stop("starpkodi17 iztrūkst apstrādes koda.")}

  if(is.na(yt$PS_code[1])) {stop("Dienas NA.")}
  yt$zinkod <- "combined"  #jo starpkodu dienu sarēķins
  return(yt)
}

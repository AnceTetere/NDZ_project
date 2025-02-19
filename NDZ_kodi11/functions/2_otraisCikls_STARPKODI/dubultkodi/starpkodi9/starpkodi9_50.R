starpkodi9_50 <- function(y, t, prev, v) {

  yt <- y[v, ]
  
  if (t$zinkod[2] %in% c("41", "51", "54", "92")) {
    if (t$zinkod[3] %in% c("40", "50", "53", "91")) {
      if (t$zinkod[4] %in% c("41", "51", "54", "92")) {
        if (t$zinkod[5] %in% c("40", "50", "53", "91")) {
          if (t$zinkod[6] %in% c("41", "51", "54", "92")) {
            if (t$zinkod[7] %in% c("40", "50", "53", "91")) {
              if (t$zinkod[8] %in% c("21", "22", "23", "24", "25", "29")) {
                if (t$zinkod[9] %in% c("41", "51", "54", "92")) {
                  if (diff(t$NDZ_sanemsanas_datums[8:9]) == 0 && all(diff(t$NDZ_sanemsanas_datums[1:8]) != 0)) {
                    yt$dd <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "dd")) - 1, 
                                     sapply(seq(2,9,by=2), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)])))
                  } else {stop("Starpkodi9_50 iztrūkst apstrādes koda.")}
                } else {stop("Starpkodi9_50 iztrūkst apstrādes koda.")} 
              } else if (t$zinkod[8] %in% c("41", "51", "54", "92")) {
                if (t$zinkod[9] %in% c("21", "22", "23", "24", "25", "29")) {
                  if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
                    yt$dd <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "dd")) - 1, 
                                     sapply(seq(2,9,by=2), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)])))
                  } else {stop("Starpkodi9_50 iztrūkst apstrādes koda.")}
                } else {stop("Starpkodi9_50 iztrūkst apstrādes koda.")} 
              } else {stop("Starpkodi9_50 iztrūkst apstrādes koda.")}
            } else {stop("Starpkodi9_50 iztrūkst apstrādes koda.")} 
          } else {stop("Starpkodi9_50 iztrūkst apstrādes koda.")} 
        } else {stop("Starpkodi9_50 iztrūkst apstrādes koda.")} 
      } else {stop("Starpkodi9_50 iztrūkst apstrādes koda.")} 
    } else {stop("Starpkodi9_50 iztrūkst apstrādes koda.")} 
  } else {stop("Starpkodi9_50 iztrūkst apstrādes koda.")} 
  
  # } else if (t$zinkod[1] %in% c("40", "50", "53", "91")) {  && 
  #            t$zinkod[2] %in% c("41", "51", "54", "92")) { && 
  #            t$zinkod[3] %in% c("40", "50", "53", "91")) {  && 
  #            t$zinkod[4] %in% c("41", "51", "54", "92")) { && 
  #            t$zinkod[5] == "91" && 
  #            t$zinkod[6] == "92" && 
  #            t$zinkod[7] == "91" && 
  #            t$zinkod[8] == "92" && 
  #            t$zinkod[9] %in% c("40", "50", "53", "91")) {  && 
  #            all(!diff(t$NDZ_sanemsanas_datums) == 0)) {
  #   dd1 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "dd")) - 1 
  #   dd2 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[3], t$NDZ_sanemsanas_datums[2], units = "dd"))
  #   dd3 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[5], t$NDZ_sanemsanas_datums[4], units = "dd"))
  #   dd4 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[7], t$NDZ_sanemsanas_datums[6], units = "dd"))
  #   dd5 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[9], t$NDZ_sanemsanas_datums[8], units = "dd"))
  #   
  #   yt <- y[v, ]
  #   yt$dd <- sum(dd1, dd2, dd3, dd4, dd5)
  #   rm(dd1, dd2, dd3, dd4, dd5)
  
  
  rm(y, t, prev, v)
  return(yt)
}

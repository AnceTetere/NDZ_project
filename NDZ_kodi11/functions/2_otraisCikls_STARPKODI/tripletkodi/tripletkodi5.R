tripletkodi5 <- function(y3, t, prev, v) {
  
  yt <- y3[v,]
  
  if (t$zinkod[1] %in% c("11", "14", "16", "61")) {
    if (t$zinkod[2] %in% c("40", "50", "53", "91")) {
      if (t$zinkod[3] %in% c("41", "51", "54", "92")) {
        if (t$zinkod[4] %in% c("41", "51", "54", "92")) {
          if (t$zinkod[5] %in% c("40", "50", "53", "91")) {
            if (all(diff(t$NDZ_sanemsanas_datums[2:5]) != 0) && diff(t$NDZ_sanemsanas_datums[1:2]) == 0) {
                 yt$dd <- sum(sapply(c(1,4), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)])))
               } else {stop("Tripletkodi5 iztrūkst apstrādes koda.")}
             } else {stop("Tripletkodi5 iztrūkst apstrādes koda.")}
           } else {stop("Tripletkodi5 iztrūkst apstrādes koda.")}
        } else {stop("Tripletkodi5 iztrūkst apstrādes koda.")}
    } else {stop("Tripletkodi5 iztrūkst apstrādes koda.")}
  } else if (t$zinkod[1] %in% c("41", "51", "54", "92")) {
    if (t$zinkod[2] %in% c("40", "50", "53", "91")) {
      if (t$zinkod[3] %in% c("21", "22", "23", "24", "25", "29")) {
        if (t$zinkod[4] %in% c("41", "51", "54", "92")) {
          if (t$zinkod[5] %in% c("41", "51", "54", "92")) {
            if (all(sapply(c(1,3), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) && 
                all(sapply(c(2,4), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
              yt$dd <- sum(sapply(c(1,3), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)])))
            } else {stop("Tripletkodi5 iztrūkst apstrādes koda.")}
          } else {stop("Tripletkodi5 iztrūkst apstrādes koda.")}
        } else {stop("Tripletkodi5 iztrūkst apstrādes koda.")}
      } else {stop("Tripletkodi5 iztrūkst apstrādes koda.")}
    } else {stop("Tripletkodi5 iztrūkst apstrādes koda.")}
  } else {stop("Tripletkodi5 iztrūkst apstrādes koda.")}

  rm(y3, t, prev, v)
  if(is.na(yt$PS_code[1])) {stop("Dienas NA.")}
  yt$zinkod <- "combined"  #jo tripletkodu dienu sarēķins
  return(yt)
}

tripletkodi4 <- function(y3, t, prev, v) {
  
  yt <- y3[v, ]
  
  if (t$zinkod[1] %in% c("21", "22", "23", "24", "25", "29")) {
    if (t$zinkod[2] %in% c("41", "51", "54", "92")) {
      if (t$zinkod[3] %in% c("41", "51", "54", "92")) {
        if (t$zinkod[4] %in% c("40", "50", "53", "91")) {
          if (all(sapply(c(1,3), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) && diff(t$NDZ_sanemsanas_datums[2:3]) != 0) {    
            if (t$period[1] == "_____" && t$PS_code[1] == "_____" && t$NM_code[1] == "__________") {
              yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[1:2]))
            } else {stop("Tripletkodi4 iztrūkst apstrādes koda.")}
          } else {stop("Tripletkodi4 iztrūkst apstrādes koda.")}
        } else {stop("Tripletkodi4 iztrūkst apstrādes koda.")}
      } else {stop("Tripletkodi4 iztrūkst apstrādes koda.")}
    } else {stop("Tripletkodi4 iztrūkst apstrādes koda.")}
  } else if (t$zinkod[1] %in% c("41", "51", "54", "92")) {
    if (t$zinkod[2] %in% c("40", "50", "53", "91")) {
      if (t$zinkod[3] %in% c("21", "22", "23", "24", "25", "29")) {
        if (t$zinkod[4] %in% c("41", "51", "54", "92")) {
          if (all(sapply(c(1,3), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) && diff(t$NDZ_sanemsanas_datums[2:3]) != 0) {    
            yt$dienas <- 0
          } else if (all(diff(t$NDZ_sanemsanas_datums[1:3]) != 0) && diff(t$NDZ_sanemsanas_datums[3:4]) == 0) {
            yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[1:2]))
          } else {stop("Tripletkodi4 iztrūkst apstrādes koda.")}
        } else {stop("Tripletkodi4 iztrūkst apstrādes koda.")}
      } else if (t$zinkod[3] %in% c("41", "51", "54", "92")) {
        if (t$zinkod[4] %in% c("21", "22", "23", "24", "25", "29")) {
          if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {    
            yt$dienas <- sum(sapply(c(1,3), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)])))
          } else {stop("Tripletkodi4 iztrūkst apstrādes koda.")}
        } else {stop("Tripletkodi4 iztrūkst apstrādes koda.")}
      } else {stop("Tripletkodi4 iztrūkst apstrādes koda.")}
    } else {stop("Tripletkodi4 iztrūkst apstrādes koda.")}
  } else if (t$zinkod[1] %in% c("40", "50", "53", "91")) {
      yt <- tripletkodi4_50(y3, t, prev, v)
  } else {stop("Tripletkodi4 iztrūkst apstrādes koda.")}

  if(is.na(yt$PS_code[1])) {stop("Dienas NA.")}
  yt$zinkod <- "combined"  #jo tripletkodu dienu sarēķins
  return(yt)
}

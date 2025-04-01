tripletkodi4_50 <- function(y3, t, prev, v) {
  
  yt <- y3[v, ]
  
    if (t$zinkod[2] %in% c("40", "50", "53", "91")) {
      if (t$zinkod[3] %in% c("21", "22", "23", "24", "25", "29")) {
        if (t$zinkod[4] %in% c("41", "51", "54", "92")) {
          if (diff(t$NDZ_sanemsanas_datums[1:2]) == 0 && all(diff(t$NDZ_sanemsanas_datums[2:4]) != 0)) {
            yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1
          } else if (all(sapply(c(1,3), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) && diff(t$NDZ_sanemsanas_datums[2:3]) != 0) {
		  yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1
          } else {stop("Tripletkodi4 iztrūkst apstrādes koda.")}
        } else {stop("Tripletkodi4 iztrūkst apstrādes koda.")}
      } else if (t$zinkod[3] %in% c("41", "51", "54", "92")) {
        if (t$zinkod[4] %in% c("21", "22", "23", "24", "25", "29")) {
          if (diff(t$NDZ_sanemsanas_datums[1:2]) == 0 && all(diff(t$NDZ_sanemsanas_datums[2:4]) != 0)) {
            yt$dienas <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1,
                             as.numeric(diff(t$NDZ_sanemsanas_datums[3:4])) + 1)
          } else {stop("Tripletkodi4 iztrūkst apstrādes koda.")}
        } else {stop("Tripletkodi4 iztrūkst apstrādes koda.")}
      } else {stop("Tripletkodi4 iztrūkst apstrādes koda.")}
    } else if (t$zinkod[2] %in% c("41", "51", "54", "92")) {
      if (t$zinkod[3] %in% c("21", "22", "23", "24", "25", "29")) {
        if (t$zinkod[4] %in% c("41", "51", "54", "92")) {
          if (all(sapply(c(1,3), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) && diff(t$NDZ_sanemsanas_datums[2:3]) != 0) {
            if (t$PS_code[1] == '_________' && t$NM_code[1] == '_________') {
              yt$dienas <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1,
                               as.numeric(diff(t$NDZ_sanemsanas_datums[2:3])))
            } else {stop("Tripletkodi4 iztrūkst apstrādes koda.")}
          } else {stop("Tripletkodi4 iztrūkst apstrādes koda.")}
        } else {stop("Tripletkodi4 iztrūkst apstrādes koda.")}
      } else if (t$zinkod[3] %in% c("41", "51", "54", "92")) {
        if (t$zinkod[4] %in% c("21", "22", "23", "24", "25", "29")) {
          if (diff(t$NDZ_sanemsanas_datums[1:2]) == 0 && all(diff(t$NDZ_sanemsanas_datums[2:4]) != 0)) {
            yt$dienas <- sum(sapply(c(1,3), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]))) + 1
          } else {stop("Tripletkodi4 iztrūkst apstrādes koda.")}
        } else {stop("Tripletkodi4 iztrūkst apstrādes koda.")}
      } else if (t$zinkod[3] %in% c("40", "50", "53", "91")) {
        if (t$zinkod[4] %in% c("21", "22", "23", "24", "25", "29")) {
          if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
            if (t$period[1] == '_____' && t$PS_code[1] =='__________' && t$NM_code[1] == '__________') {
              yt$dienas <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1,
                               as.numeric(diff(t$NDZ_sanemsanas_datums[2:3])))
            } else {stop("Tripletkodi4 iztrūkst apstrādes koda.")}
          } else {stop("Tripletkodi4 iztrūkst apstrādes koda.")}
        } else {stop("Tripletkodi4 iztrūkst apstrādes koda.")}
      } else {stop("Tripletkodi4 iztrūkst apstrādes koda.")}
    } else {stop("Tripletkodi4 iztrūkst apstrādes koda.")}
  
  rm(y3, t, prev, v)
  return(yt)
}

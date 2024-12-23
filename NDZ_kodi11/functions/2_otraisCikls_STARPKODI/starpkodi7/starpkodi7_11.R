starpkodi7_11 <- function(y, t, prev, v) {
  
  if (t$zk[2] %in% c("40", "50", "53", "91")) {
    if (t$zk[3] %in% c("41", "51", "54", "92")) {
      if (t$zk[4] %in% c("40", "50", "53", "91")) {
        if (t$zk[5] %in% c("41", "51", "54", "92")) {
          if (t$zk[6] %in% c("40", "50", "53", "91")) {
            if (t$zk[7] %in% c("41", "51", "54", "92")) {
              if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
                yt <- y[v, ]
                yt$dd <- sum(sapply(c(1,3,5), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)])),
                                 difftime(t$last_date[7], t$NDZ_sanemsanas_datums[7], units = "days") + 1)
              } else {stop("Starpkodi7_11: Trūkst izstrādes koda.")}
            } else if (t$zk[7] %in% c("21", "22", "23", "24", "25", "29")) {
              if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
                yt <- y[v, ]
                yt$dd <- sum(sapply(c(1,3,5), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]))) 
              } else {stop("Starpkodi7_11: Trūkst izstrādes koda.")}
            } else {stop("Starpkodi7_11: Trūkst izstrādes koda.")}
          } else {stop("Starpkodi7_11: Trūkst izstrādes koda.")}
        } else {stop("Starpkodi7_11: Trūkst izstrādes koda.")}
      } else {stop("Starpkodi7_11: Trūkst izstrādes koda.")}
    } else {stop("Starpkodi7_11: Trūkst izstrādes koda.")}
  } else {stop("Starpkodi7_11: Trūkst izstrādes koda.")}



  if(is.na(yt$PS_code[1])) {stop("DD NA.")}
  return(yt) 
}

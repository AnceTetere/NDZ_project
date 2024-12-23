starpkodi7_51_50 <- function(y, t, prev, v) {
  
    if (t$zk[3] %in% c("40", "50", "53", "91")) {
      if (t$zk[4] %in% c("41", "51", "54", "92")) {
        if (t$zk[5] %in% c("21", "22", "23", "25", "24", "29")) {
          if (t$zk[6] %in% c("41", "51", "54", "92")) {
            if (t$zk[7] %in% c("41", "51", "54", "92")) {
              if (all(sapply(c(1,2,4,5,6), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0)) && 
                  diff(t$NDZ_sanemsanas_datums[3:4]) == 0) {
                if (t$PS_code[1] == '___________' && t$NM_code[1] == '___________') {
                  yt <- y[v, ]
                  yt$dd <- as.numeric(sum(diff(t$NDZ_sanemsanas_datums[1:2])),
                                          diff(t$NDZ_sanemsanas_datums[c(3,5)]) + 1)
                } else {stop("starpkodi7_51_50 iztrūkst apstrādes koda.")}
              } else {stop("starpkodi7_51_50 iztrūkst apstrādes koda.")}
            } else {stop("starpkodi7_51_50 iztrūkst apstrādes koda.")}
          } else {stop("starpkodi7_51_50 iztrūkst apstrādes koda.")}
        } else {stop("starpkodi7_51_50 iztrūkst apstrādes koda.")}
      } else {stop("starpkodi7_51_50 iztrūkst apstrādes koda.")}
    } else if (t$zk[3] %in% c("41", "51", "54", "92")) {
      if (t$zk[4] %in% c("41", "51", "54", "92")) {
        if (t$zk[5] %in% c("40", "50", "53", "91")) {
          if (t$zk[6] %in% c("41", "51", "54", "92")) {
            if (t$zk[7] %in% c("40", "50", "53", "91")) {
              if (all(sapply(c(1,3,4,5,6), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0)) && 
                  diff(t$NDZ_sanemsanas_datums[2:3]) == 0) {
                yt <- y[v, ]
                yt$dd <- sum(sapply(c(1,4,6), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]))) + 1
              } else {stop("starpkodi7_51_50 iztrūkst apstrādes koda.")}
            } else {stop("starpkodi7_51_50 iztrūkst apstrādes koda.")}
          } else {stop("starpkodi7_51_50 iztrūkst apstrādes koda.")}
        } else {stop("starpkodi7_51_50 iztrūkst apstrādes koda.")}
      } else if (t$zk[4] %in% c("40", "50", "53", "91")) {
        if (t$zk[5] %in% c("41", "51", "54", "92")) {
          if (t$zk[6] %in% c("40", "50", "53", "91")) {
            if (t$zk[7] %in% c("21", "22", "23", "25", "24", "29")) {
              if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
                yt <- y[v, ]
                yt$dd <- sum(sapply(seq(1,6,by=2), function(i) as.numeric(diff(t$NDZ_sanemsanas_datums[i:(i+1)]))))
              } else {stop("starpkodi7_51_50 iztrūkst apstrādes koda.")}
            } else {stop("starpkodi7_51_50 iztrūkst apstrādes koda.")}
          } else {stop("starpkodi7_51_50 iztrūkst apstrādes koda.")}
        } else {stop("starpkodi7_51_50 iztrūkst apstrādes koda.")}
      } else {stop("starpkodi7_51_50 iztrūkst apstrādes koda.")}
    } else {stop("starpkodi7_51_50 iztrūkst apstrādes koda.")}
    
  rm(y, t, prev, v)
  return(yt)
}

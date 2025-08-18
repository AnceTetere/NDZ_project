starpkodi7_51_11 <- function(y, t, prev, v) {
  
  if (t$zk[3] %in% c("21", "22", "23", "24", "25", "29")) {
    if (t$zk[4] %in% c("40", "50", "53", "91")) {
      if (t$zk[5] %in% c("41", "51", "54", "92")) {
        if (t$zk[6] %in% c("11", "14", "16", "61")) {
          if (t$zk[7] %in% c("21", "22", "23", "24", "25", "29")) {
            if (all(sapply(c(2,4,6), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) && 
                all(sapply(c(1,3,5), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
              if (t$PS_code[1] == '_________' && t$NM_code[1] == "_________" && t$period[1] == "202101") {
                yt <- y[v, ]
                yt$dienas <- sum(as.numeric(diff(t$NDZ_sanemsanas_datums[c(1,3)])), 
                                 as.numeric(diff(t$NDZ_sanemsanas_datums[c(5,7)])))
              } else {stop("Starpkodi7_51_11 iztrūkst apstrādes koda.")}
            } else {stop("Starpkodi7_51_11 iztrūkst apstrādes koda.")}
          } else {stop("Starpkodi7_51_11 iztrūkst apstrādes koda.")}
        } else {stop("Starpkodi7_51_11 iztrūkst apstrādes koda.")}
      } else {stop("Starpkodi7_51_11 iztrūkst apstrādes koda.")}
    } else {stop("Starpkodi7_51_11 iztrūkst apstrādes koda.")}
  } else {stop("Starpkodi7_51_11 iztrūkst apstrādes koda.")}
  
  
  rm(y, t, prev, v)
  return(yt)
}

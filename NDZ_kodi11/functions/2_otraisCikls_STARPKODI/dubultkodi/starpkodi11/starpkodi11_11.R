starpkodi11_11 <- function(y, t, prev, v) {
  
  yt <- y[v, ]
  
    if (t$zinkod[2] %in% c("40", "50", "53", "91")) {
      if (t$zinkod[3] %in% c("41", "51", "54", "92")) {
        if (t$zinkod[4] %in% c("40", "50", "53", "91")) {
          if (all(t$zinkod[seq(6,11,by=2)] %in% c("40", "50", "53", "91")) && 
              all(t$zinkod[seq(5,11,by=2)] %in% c("41", "51", "54", "92")) && 
              all(diff(t$NDZ_sanemsanas_datums) != 0)) {
            yt$dienas <- sum(sapply(seq(1, 10, by=2), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)])),
                             difftime(t$last_date[11], t$NDZ_sanemsanas_datums[11], units = "days")) + 1
          } else {stop("Starpkodi11 iztrūkst apstrādes koda.")}
        } else if (t$zinkod[4] %in% c("41", "51", "54", "92")) {
          if (all(t$zinkod[seq(6,11,by=2)] %in% c("41", "51", "54", "92")) && 
              all(t$zinkod[seq(5,11,by=2)] %in% c("40", "50", "53", "91")) && 
              all(diff(t$NDZ_sanemsanas_datums) != 0)) {
            if (t$period[1] == '______' && t$PS_code[1] ==  '______________' &&  t$NM_code[1] ==  '______________') {
              yt$dienas <- sum(sapply(c(1,4,6,8,10), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)])))
            } else {stop("Starpkodi11 iztrūkst apstrādes koda.")}
          } else {stop("Starpkodi11 iztrūkst apstrādes koda.")}
        } else {stop("Starpkodi11 iztrūkst apstrādes koda.")}
      } else {stop("Starpkodi11 iztrūkst apstrādes koda.")}
    } else {stop("Starpkodi11 iztrūkst apstrādes koda.")}
  
  rm(y, t, prev, v)
  return(yt)
}

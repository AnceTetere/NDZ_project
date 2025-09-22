starpkodi3_50_11 <- function(y, t, prev, v) {

  yt <- y[v, ]
  
  if (t$zinkod[3]  %in% c("41", "51", "54", "92")) {
    if (diff(t$NDZ_sanemsanas_datums[1:2]) != 0 && diff(t$NDZ_sanemsanas_datums[2:3]) == 0) {
        if (t$period[1] == '______' && t$PS_code[1] ==  '______________' && t$NM_code[1] ==  '______________') {
          yt <- y[v:(v+1), ]
          yt <- yt[yt$zinkod == "50", ]
      } else {stop("starpkodi3_50_11: Iztrūkst apstrādes koda.")}
    } else if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
      if ((t$period[1] == '______' && t$PS_code[1] ==  '______________' && t$NM_code[1] ==  '______________') ||
          (t$period[1] == '______' && t$PS_code[1] ==  '______________' && t$NM_code[1] ==  '______________')) {
        yt$dienas <- as.numeric(sum(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days") - 1,
                                    difftime(t$last_date[3], t$NDZ_sanemsanas_datums[3], units = "days") + 1))
      } else {stop("starpkodi3_50_11: Iztrūkst apstrādes koda.")}
    } else {stop("starpkodi3_50_11: Iztrūkst apstrādes koda.")}
  } else {stop("starpkodi3_50_11: Iztrūkst apstrādes koda.")}
               
  rm(y, t, prev, v)
  return(yt) 
}

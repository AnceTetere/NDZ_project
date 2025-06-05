starpkodi5_50_25 <- function(y, t, prev, v) {
  
  yt <- y[v,]
  
  if (t$zinkod[3] %in% c("41", "51", "54", "92")) {
    if (t$zinkod[4] %in% c("41", "51", "54", "92")) {
      if (t$zinkod[5] %in% c("41", "51", "54", "92")) {
        if (NDZ$period[1] == '______' && t$PS_code[1] ==  '______________' && t$NM_code[1] ==  '______________') {
          yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1 
        } else {stop("Starpkodi5_50_25: iztrūkst apstrādes koda.")} 
      } else {stop("Starpkodi5_50_25: iztrūkst apstrādes koda.")}
    } else {stop("Starpkodi5_50_25: iztrūkst apstrādes koda.")}
  } else {stop("Starpkodi5_50_25: iztrūkst apstrādes koda.")}
  
  
  rm(y, t, prev, v)
  return(yt)
}

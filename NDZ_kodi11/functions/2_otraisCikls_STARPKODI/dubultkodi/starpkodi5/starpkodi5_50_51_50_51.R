starpkodi5_50_51_50_51 <- function(y, t, prev, v) {
  
  yt <- y[v, ]
  

  if (t$zinkod[5] %in% c("21", "22", "23", "24", "25", "29")) {
      if (diff(t$NDZ_sanemsanas_datums[3:4]) == 0 && all(sapply(c(1,2,4), function (i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
        yt$dienas <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1, 
                         as.numeric(diff(t$NDZ_sanemsanas_datums[3:5])))
      } else if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
        yt$dienas <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days") - 1), 
                         sapply(c(2,4), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)])))
      } else if (diff(t$NDZ_sanemsanas_datums[2:3]) == 0 &&
                 all(sapply(c(1,3,4), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
        yt$dienas <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days") - 1), 
                         sapply(c(2,4), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)])))
      } else {stop("starpkodi5_50_51_50_51: iztrūkst apstrādes koda.")}
  } else if (t$zinkod[5] %in% c("40", "50", "53", "91")) {
      if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
        yt$dienas <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1, 
                         sapply(c(2,4), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)])))
      } else {stop("starpkodi5_50_51_50_51: iztrūkst apstrādes koda.")}
  } else {stop("starpkodi5_50_51_50_51: iztrūkst apstrādes koda.")}
    
  rm(y, t, prev, v)
  return(yt)
}

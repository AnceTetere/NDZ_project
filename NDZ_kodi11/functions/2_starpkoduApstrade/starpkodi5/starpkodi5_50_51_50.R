starpkodi5_50_51_50 <- function(y, t, prev, v) {
  
  if (t$zinkod[4] == "51") {
    if (t$zinkod[5] == "21") {
      if (diff(t$NDZ_sanemsanas_datums[3:4]) == 0 && all(sapply(c(1,2,4), function (i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
        yt <- y[v, ]
        yt$dienas <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1, 
                         as.numeric(diff(t$NDZ_sanemsanas_datums[3:5])))
      } else {stop("Starpkodi5_50_51: iztrūkst apstrādes koda.")}
    } else {stop("Starpkodi5_50_51: iztrūkst apstrādes koda.")}
  } else if (t$zinkod[4] == "25") {
    if (t$zinkod[5] == "51") {
      if (all(diff(t$NDZ_sanemsanas_datums[1:4]) != 0) && diff(t$NDZ_sanemsanas_datums[4:5]) == 0) {
        yt <- y[v, ]
        yt$dienas <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1, 
                         as.numeric(diff(t$NDZ_sanemsanas_datums[2:3])))
      } else {stop("Starpkodi5_50_51: iztrūkst apstrādes koda.")}
    } else {stop("Starpkodi5_50_51: iztrūkst apstrādes koda.")}
  } else if(t$zinkod[4] == "51") {
    if(t$zinkod[5] == "25") {
      if(all(diff(t$NDZ_sanemsanas_datums) != 0)) {
        yt <- y[v, ]
        yt$dienas <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1, 
                         as.numeric(diff(t$NDZ_sanemsanas_datums[2:3])),
                         as.numeric(diff(t$NDZ_sanemsanas_datums[4:5])))
      } else {stop("Starpkodi5_50_51: iztrūkst apstrādes koda.")}
    } else {stop("Starpkodi5_50_51: iztrūkst apstrādes koda.")}
  } else if(t$zinkod[4] == "21") {
    if(t$zinkod[5] == "51") {
      if(all(diff(t$NDZ_sanemsanas_datums[1:4]) != 0) && diff(t$NDZ_sanemsanas_datums[4:5]) == 0) {
        yt <- y[v, ]
        yt$dienas <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1, 
                         as.numeric(diff(t$NDZ_sanemsanas_datums[2:3])))
      } else {stop("Starpkodi5_50_51: iztrūkst apstrādes koda.")}
    } else {stop("Starpkodi5_50_51: iztrūkst apstrādes koda.")}
  } else {stop("Starpkodi5_50_51: iztrūkst apstrādes koda.")}
  
  return(yt)
}

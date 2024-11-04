starpkodi3_50_25 <- function(y, t, prev, v) {
    
  if (t$zinkod[3] == "11") {
           if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
             yt <- y[v, ]
             yt$dienas <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1, 
                              as.numeric(difftime(t$last_date[3], t$NDZ_sanemsanas_datums[3], units = "days")) + 1)
           } else {stop("starpkodi3_50_25: Iztrūkst apstrādes koda.")}
  } else if (t$zinkod[3] == "51") {
           yt <- y[v, ]
           yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1
  } else {stop("starpkodi3_50_25: Iztrūkst apstrādes koda.")} 

  rm(y, t, prev, v)
  return(yt)
}

starpkodi3_25_11 <- function(y, t, prev, v) {
  
  yt <- y[v, ]
  
  if (t$zinkod[3] %in% c("40", "50", "53", "91")) {
    if (all(diff(t$NDZ_sanemsanas_datums) != 0))  {
      yt$dienas <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")),
                       as.numeric(diff(t$NDZ_sanemsanas_datums[2:3])))
    } else if (diff(t$NDZ_sanemsanas_datums[2:3]) == 0 && diff(t$NDZ_sanemsanas_datums[1:2]) != 0) {
      yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days"))
    } else {stop("Starpkodi3_25_11: Trūkst izstrādes koda.")}
  } else {stop("Starpkodi3_25_11: Trūkst izstrādes koda.")} 
  
  rm(y, t, prev, v)
  return(yt)
}

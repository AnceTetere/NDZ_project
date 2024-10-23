starpkodi2_41 <- function(y, t, prev, v) {
  
  if (t$zinkod[2] %in% c("21", "25")) {
    yt <- y[v,] 
    yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[1:2])) + 1
  } else if (t$zinkod[2] == "50") {
    yt <- y2[v,] 
    yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[1:2]))
  #} else if (t$zinkod[2] == "53") {
    #  yt <- y2[v,] 
    #  yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[2], t$NDZ_sanemsanas_datums[1], units = "days"))
  } else {stop("Starpkodi2_41: Trūkst izstrādes koda.")}
  
  
  return(yt)  
}

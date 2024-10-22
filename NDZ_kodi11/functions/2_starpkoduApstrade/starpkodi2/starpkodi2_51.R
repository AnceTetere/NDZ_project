starpkodi2_51 <- function(y, t, prev, v) {
  if (t$zinkod[2] %in% c("21", "22", "23", "25")) {
    yt <- y[v,]
    yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[1:2]) + 1)
  } else if (t$zinkod[2] %in% c("40", "53", "91")) {
    yt <- y[v,] 
    yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[1:2]))
  #  
  #} else if (t$zinkod[2] == "41" && t$PS_code[1] == '__________' && t$NM_code[1] == '___________') {
  #  days1 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days"))
  #  days2 <- as.numeric(difftime(t$last_date[2], t$NDZ_sanemsanas_datums[2], units = "days")) 
    
  #  yt <- y2[v,] 
  #  yt$dienas <- sum(days1, days2)
  #  rm(days1, days2)
  #} else if (t$zinkod[2] == "11" && t$PS_code[1] == '__________' && t$NM_code[1] == '__________') {
  #  yt <- y2[v,] 
  #  yt$dienas <- 0
  #} else if (t$zinkod[2] == "11" && t$PS_code[1] == '__________' && t$NM_code[1] == '__________') {
  #  days1 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days"))
  #  days2 <- as.numeric(difftime(t$last_date[2], t$NDZ_sanemsanas_datums[2], units = "days"))
                                 
  #  yt <- y2[v,] 
  #  yt$dienas <- sum(days1, days2)
  #  rm(days1, days2)
  #} else if (t$zinkod[2] == "92") {
  #  yt <- y2[v,] 
  #  yt$dienas <- as.numeric(difftime(t$last_date[2], t$NDZ_sanemsanas_datums[2], units = "days")) + 1
  } else {stop("starpkodi2_51: Trūkst izstrādes koda.")}
  
  return(yt)
}

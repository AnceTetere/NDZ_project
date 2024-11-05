starpkodi2_40 <- function(y, t, prev, v) {

  if (t$zinkod[2] %in% c("21", "22", "23", "24", "25", "29")) {
      yt <- y[v,]
      yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1
  } else {stop("Starpkodi2_40: Trūkst izstrādes koda.")}
  
  rm(y, t, prev, v)
  return(yt)
}

  
#           t$PS_code[1] == '______________' && t$NM_code[1] == '______________') {
#  yt <- y2[v,] 
#  yt$dienas <- as.numeric(difftime(t$beidz_darbu[1], prev, units = "days")) - 1 

#} else if (t$zinkod[1] == "40" && t$zinkod[2] == "51" && t$PS_code[1] == '______________' && t$NM_code[1] == '______________') {
#  days1 <- as.numeric(difftime(t$beidz_darbu[1], prev, units = "days")) -1 
#  days2 <- as.numeric(difftime(t$last_date[2], t$sak_darbu[2], units = "days")) + 1 

#  yt <- y2[v,] 
#  yt$dienas <- sum(days1, days2)
#  rm(days1,days2)

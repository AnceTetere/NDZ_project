starpkodi5_50 <- function(y, t, prev, v) {
  
  if (t$zinkod[2] == "51") {
    yt <- starpkodi5_50_51(y, t, prev, v)
  } else if(t$zinkod[2] == "25" && 
          t$zinkod[3] == "51" && t$zinkod[4] == "11" && 
          t$zinkod[5] == "25" && 
          all(!diff(t$NDZ_sanemsanas_datums[3:5]) == 0) &&
          t$NDZ_sanemsanas_datums[2] == t$NDZ_sanemsanas_datums[3] &&
          t$NDZ_sanemsanas_datums[1] != t$NDZ_sanemsanas_datums[2]) {
  days1 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) #atvaļinājums, bet atgriežoties tiek atlaists tur summējas viena diena, tāpēc šeit to neatņemu
  days2 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[5], t$NDZ_sanemsanas_datums[4], units = "days")) + 1 
  
  yt <- y[v, ]
  yt$dienas <- sum(days1, days2)
  rm(days1, days2)
} else if(all(t$zinkod[1:3] == "50") && t$zinkod[5] == "51" && t$zinkod[4] == "25" && 
          all(diff(t$NDZ_sanemsanas_datums[1:4]) != 0) && diff(t$NDZ_sanemsanas_datums[4:5]) == 0) {
  yt <- y[v, ]
  yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[3], prev, units = "days"))
} else if(t$zinkod[2] == "21" && all(t$zinkod[3:5] == "51")) {
  yt <- y[v, ]
  yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1 
} else if(t$zinkod[2] == "91" && t$zinkod[3] == "51" && t$zinkod[4] == "92" && t$zinkod[5] == "50" &&
          diff(t$NDZ_sanemsanas_datums[2:3]) == 0 &&
          all(sapply(c(1,3,4), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
  days1 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1 
  days2 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[2], t$NDZ_sanemsanas_datums[3], units = "days"))
  days3 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[5], t$NDZ_sanemsanas_datums[4], units = "days"))
  
  yt <- y[v, ]
  yt$dienas <- sum(days1, days2, days3) 
  rm(days1, days2, days3)
} else {stop("Starpkodi5_50: iztrūkst apstrādes koda.")}

  if(is.na(yt$PS_code[1])) {stop("Dienas NA.")}
return(yt)
}

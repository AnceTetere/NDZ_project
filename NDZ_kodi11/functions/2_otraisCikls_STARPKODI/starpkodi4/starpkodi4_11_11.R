starpkodi4_11_11 <- function(y2, t, prev, v) {
  
  if (t$zinkod[3] %in% c("40", "50", "53", "91")) {
    if (t$zinkod[4] %in% c("41", "51", "54", "92")) {
      if ((all(diff(t$NDZ_sanemsanas_datums) != 0)) ||
          (all(sapply(c(1,3), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0)) &&
           diff(t$NDZ_sanemsanas_datums[2:3]) == 0)) {
        yt <- y2[v, ]
        yt$dienas <- as.numeric(sum(diff(t$NDZ_sanemsanas_datums[2:3]),
                                    difftime(t$last_date[4], t$NDZ_sanemsanas_datums[4], units = "days")) + 1)
      } else {stop("Starpkodi4_11_11: Trūkst izstrādes koda.")}
    } else {stop("Starpkodi4_11_11: Trūkst izstrādes koda.")}
  } else {stop("Starpkodi4_11_11: Trūkst izstrādes koda.")}

  rm(y2, t, prev, v)
  return(yt) 
}

    
  
    
    
    
    
    #} else if (t$zinkod[3] == "40" && t$zinkod[4] == "11" && all(diff(t$NDZ_sanemsanas_datums[2:4]) != 0) && diff(t$NDZ_sanemsanas_datums[1:2]) == 0 &&
  #           t$PS_code[1] == '__________' && t$NM_code[1] == '__________') {
  #  yt <- y2[v, ]
  #  yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[3], t$NDZ_sanemsanas_datums[2], units = "days"))
  #} else if (t$zinkod[3] == "50" && t$zinkod[4] == "51" &&all(diff(t$NDZ_sanemsanas_datums[2:4]) != 0) && diff(t$NDZ_sanemsanas_datums[1:2]) == 0) {
   # days1 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[3], t$NDZ_sanemsanas_datums[2], units = "days"))
  #  days2 <- as.numeric(difftime(t$last_date[4], t$NDZ_sanemsanas_datums[4], units = "days")) + 1
    
   # yt <- y2[v, ]
  #  yt$dienas <- sum(days1, days2)
   # rm(days1,days2)
  #} else if (t$zinkod[3] == "50" && t$zinkod[4] == "51" && all(diff(t$NDZ_sanemsanas_datums[1:3]) == 0) && diff(t$NDZ_sanemsanas_datums[3:4]) != 0) {
   # days1 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[3], t$NDZ_sanemsanas_datums[2], units = "days"))
  #  days2 <- as.numeric(difftime(t$last_date[4], t$NDZ_sanemsanas_datums[4], units = "days")) + 1
    
   # yt <- y2[v, ]
  #  yt$dienas <- sum(days1, days2)
   # rm(days1,days2)
  #} else if (t$zinkod[3] == "40" && t$zinkod[4] == "40" && all(diff(t$NDZ_sanemsanas_datums) == 0)) {
  #  yt <- y2[v, ]
  #  yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[4], t$NDZ_sanemsanas_datums[1], units = "days"))
  
  

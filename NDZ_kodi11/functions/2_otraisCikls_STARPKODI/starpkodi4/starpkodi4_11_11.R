starpkodi4_11_11 <- function(y, t, prev, v) {
  
  yt <- y[v, ]
  
  if (t$zinkod[3] %in% c("40", "50", "53", "91")) {
    if (t$zinkod[4] %in% c("41", "51", "54", "92")) {
      if ((all(diff(t$NDZ_sanemsanas_datums) != 0)) ||
          (all(sapply(c(1,3), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0)) &&
           diff(t$NDZ_sanemsanas_datums[2:3]) == 0)) {
        yt$dd <- as.numeric(sum(diff(t$NDZ_sanemsanas_datums[2:3]),
                                    difftime(t$last_date[4], t$NDZ_sanemsanas_datums[4], units = "days")) + 1)
      } else {stop("Starpkodi4_11_11: Trūkst izstrādes koda.")}
    } else {stop("Starpkodi4_11_11: Trūkst izstrādes koda.")}
  } else if (t$zinkod[3] %in% c("21", "22", "23", "24", "25", "29")) {
    if (t$zinkod[4] %in% c("40", "50", "53", "91")) {
      if (all(diff(t$NDZ_sanemsanas_datums[1:3]) != 0) && diff(t$NDZ_sanemsanas_datums[3:4]) == 0) {
          if (t$period[1] == "_______" && t$PS_code[1] == "_______" && t$NM_code[1] == "_______") {
            yt$dd <- as.numeric(diff(t$NDZ_sanemsanas_datums[2:3])) + 1
          } else {stop("Starpkodi4_11_11: Trūkst izstrādes koda.")}
      } else {stop("Starpkodi4_11_11: Trūkst izstrādes koda.")}
    } else {stop("Starpkodi4_11_11: Trūkst izstrādes koda.")}
  } else if (t$zinkod[3] %in% c("11", "14", "16", "61")) {
    if (t$zinkod[4] %in% c("40", "50", "53", "91")) {
      if (all(diff(t$NDZ_sanemsanas_datums[1:3]) != 0) && diff(t$NDZ_sanemsanas_datums[3:4]) == 0) {
        if (t$period[1] == "________" && t$PS_code[1] == "_______" && t$NM_code[1] == "_______") {
          yt$dd <- as.numeric(diff(t$NDZ_sanemsanas_datums[3:4]))
        } else {stop("Starpkodi4_11_11: Trūkst izstrādes koda.")}
      } else {stop("Starpkodi4_11_11: Trūkst izstrādes koda.")}
    } else {stop("Starpkodi4_11_11: Trūkst izstrādes koda.")}
  } else {stop("Starpkodi4_11_11: Trūkst izstrādes koda.")}

  rm(y, t, prev, v)
  return(yt) 
}

    
  
    
    
    
    
    #} else if (t$zinkod[3] == "40" && t$zinkod[4] == "11" && all(diff(t$NDZ_sanemsanas_datums[2:4]) != 0) && diff(t$NDZ_sanemsanas_datums[1:2]) == 0 &&
  #           t$PS_code[1] == '_______' && t$NM_code[1] == '_______') {
  #  yt <- y[v, ]
  #  yt$dd <- as.numeric(difftime(t$NDZ_sanemsanas_datums[3], t$NDZ_sanemsanas_datums[2], units = "days"))
  #} else if (t$zinkod[3] == "50" && t$zinkod[4] == "51" &&all(diff(t$NDZ_sanemsanas_datums[2:4]) != 0) && diff(t$NDZ_sanemsanas_datums[1:2]) == 0) {
   # days1 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[3], t$NDZ_sanemsanas_datums[2], units = "days"))
  #  days2 <- as.numeric(difftime(t$last_date[4], t$NDZ_sanemsanas_datums[4], units = "days")) + 1
    
   # yt <- y[v, ]
  #  yt$dd <- sum(days1, days2)
   # rm(days1,days2)
  #} else if (t$zinkod[3] == "50" && t$zinkod[4] == "51" && all(diff(t$NDZ_sanemsanas_datums[1:3]) == 0) && diff(t$NDZ_sanemsanas_datums[3:4]) != 0) {
   # days1 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[3], t$NDZ_sanemsanas_datums[2], units = "days"))
  #  days2 <- as.numeric(difftime(t$last_date[4], t$NDZ_sanemsanas_datums[4], units = "days")) + 1
    
   # yt <- y[v, ]
  #  yt$dd <- sum(days1, days2)
   # rm(days1,days2)
  #} else if (t$zinkod[3] == "40" && t$zinkod[4] == "40" && all(diff(t$NDZ_sanemsanas_datums) == 0)) {
  #  yt <- y[v, ]
  #  yt$dd <- as.numeric(difftime(t$NDZ_sanemsanas_datums[4], t$NDZ_sanemsanas_datums[1], units = "days"))

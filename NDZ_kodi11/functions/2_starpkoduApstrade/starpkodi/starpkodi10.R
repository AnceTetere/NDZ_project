starpkodi10 <- function(y, t, prev, v) {
  
  yt <- y[v, ]
  
  if (t$zinkod[1] == "11" && 
      all(t$zinkod[c(2,5,8)] %in% c("40", "50", "53", "91")) && 
      all(t$zinkod[c(3,4,6,7,9,10)] %in% c("41", "51", "54", "92"))) {
             if (all(diff(t$NDZ_sanemsanas_datums[c(1:7,9:10)]) != 0) && diff(t$NDZ_sanemsanas_datums[8:9]) == 0) {
               yt$dd <- sum(sapply(c(1,4,7), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)])),
                                difftime(t$last_date[10], t$NDZ_sanemsanas_datums[10], units = "days") + 1)
             } else {stop("Starpkodi10 iztrūkst apstrādes koda.")}
  } else if (all(t$zinkod[c(1,3,5,7,9)] %in% c("41", "51", "54", "92"))) {
    if (all(t$zinkod[c(2,4,6,8)] %in% c("40", "50", "53", "91"))) {
      if (t$zinkod[10] %in% c("21", "22", "23", "24", "25", "29")) {
        yt$dd <- sum(sapply(seq(1,10,by=2), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]))) + 1
      } else {stop("Starpkodi10 iztrūkst apstrādes koda.")}
    } else {stop("Starpkodi10 iztrūkst apstrādes koda.")}
  } else {stop("Starpkodi10 iztrūkst apstrādes koda.")}

  
  
  
  #if (t$zinkod[1] == "51" && 
  #    t$zinkod[2] == "50" && 
  #    t$zinkod[3] == "51" && 
  #    t$zinkod[4] == "50" && 
  #    t$zinkod[5] == "51" && 
  #    t$zinkod[6] == "50" && 
  #    t$zinkod[7] == "51" && 
  #    t$zinkod[8] == "50" && 
  #    t$zinkod[9] %in% c("21", "22", "23", "24", "25", "29") && 
  #    t$zinkod[10] == "51" && 
  #    all(!diff(t$NDZ_sanemsanas_datums[1:9]) == 0) &&
  #    t$NDZ_sanemsanas_datums[9] == t$NDZ_sanemsanas_datums[10]) {
  #  yt <- y[v:(v+1), ]
  #  yt <- yt[yt$zinkod == "50", ]
  #} else if (t$zinkod[1] == "51" && 
  #           t$zinkod[2] == "50" && 
  #           t$zinkod[3] == "51" && 
  #           t$zinkod[4] == "50" && 
  #           t$zinkod[5] == "51" && 
  #           t$zinkod[6] == "50" && 
  #           t$zinkod[7] == "51" && 
  #           t$zinkod[8] == "50" && 
  #           t$zinkod[9] == "51" && 
  #           t$zinkod[10] == "25" && 
  #           all(diff(t$NDZ_sanemsanas_datums) != 0)) {
  #  days1 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[2], t$NDZ_sanemsanas_datums[1], units = "days"))
  #  days2 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[4], t$NDZ_sanemsanas_datums[3], units = "days"))
  #  days3 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[6], t$NDZ_sanemsanas_datums[5], units = "days"))
  #  days4 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[8], t$NDZ_sanemsanas_datums[7], units = "days"))
  #  days5 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[10], t$NDZ_sanemsanas_datums[9], units = "days")) + 1
  #  
  #  yt <- y[v, ]
  #  yt$dd <- sum(days1, days2, days3, days4, days5)
  #  rm(days1, days2, days3, days4, days5)
  #} else {stop("Starpkodi10 iztrūkst apstrādes koda.")}
  
  if(is.na(yt$PS_code[1])) {stop("Dienas NA.")}
  yt$zinkod <- "combined"  
  return(yt)
}

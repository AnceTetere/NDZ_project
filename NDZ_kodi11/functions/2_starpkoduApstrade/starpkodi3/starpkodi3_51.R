starpkodi3_51 <- function(y, t, prev, v) {
  
  if (t$zinkod[2] == "50") {
    yt <- starpkodi3_51_50(y, t, prev, v)
  } else if (t$zinkod[2] == "51") {
    if (t$zinkod[3] == "25") {
      if(all(diff(t$NDZ_sanemsanas_datums) != 0)) {
        yt <- y2[v, ]
        yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[2:3])) + 1 
      } else {stop("Starpkodi3_51: Trūkst izstrādes koda.")}
    } else {stop("Starpkodi3_51: Trūkst izstrādes koda.")}

  #} else if (t$zinkod[2] == "21" && t$zinkod[3] == "11" && all(!diff(t$NDZ_sanemsanas_datums) == 0)) {
  #  days1 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[2], t$NDZ_sanemsanas_datums[1], units = "days")) + 1  
  #  days2 <- as.numeric(difftime(t$last_date[3], t$NDZ_sanemsanas_datums[3], units = "days"))  + 1 
  #  
  #  yt <- y2[v, ]
  #  yt$dienas <- sum(days1, days2)
  #  rm(days1, days2)
  #} else if (t$zinkod[2] == "25" && t$zinkod[3] == "50" && 
  #           all(!diff(t$NDZ_sanemsanas_datums[1:2]) == 0) && t$NDZ_sanemsanas_datums[2] == t$NDZ_sanemsanas_datums[3]) {
  #  yt <- y2[v, ]
  #  yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[2], t$NDZ_sanemsanas_datums[1], units = "days")) #bez '+ 1' jo ambiguous
  #} else if (t$zinkod[2] == "25" && t$zinkod[3] == "11" && all(diff(t$NDZ_sanemsanas_datums) != 0)) {
  #  days1 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[2], t$NDZ_sanemsanas_datums[1], units = "days")) + 1  
  #  days2 <- as.numeric(difftime(t$last_date[3], t$NDZ_sanemsanas_datums[3], units = "days"))  + 1 
  #  
  #  yt <- y2[v, ]
  #  yt$dienas <- sum(days1, days2)
  #  rm(days1, days2)
  #} else if (t$zinkod[2] == "91" && t$zinkod[3] == "92" && all(diff(t$NDZ_sanemsanas_datums) != 0)) {
  #  days1 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[2], t$NDZ_sanemsanas_datums[1], units = "days"))
  #  days2 <- as.numeric(difftime(t$last_date[3], t$NDZ_sanemsanas_datums[3], units = "days"))  + 1 
  #  
  #  yt <- y2[v, ]
  #  yt$dienas <- sum(days1, days2)
  #  rm(days1, days2)
  #} else if (t$zinkod[2] == "25" && t$zinkod[3] == "51" && all(diff(t$NDZ_sanemsanas_datums[1:2]) != 0) && all(diff(t$NDZ_sanemsanas_datums[2:3]) == 0)) {
  #  yt <- y2[v, ]
  #  yt$dienas <- 1
  #} else if (t$zinkod[2] == "53" && t$zinkod[3] == "54" && all(diff(t$NDZ_sanemsanas_datums) != 0)) {
  #  days1 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[2], t$NDZ_sanemsanas_datums[1], units = "days"))
  #  days2 <- as.numeric(difftime(t$last_date[3], t$NDZ_sanemsanas_datums[3], units = "days")) + 1
  #  
  #  yt <- y2[v, ]
  #  yt$dienas <- sum(days1,days2)
  #  rm(days1,days2)
  #} else if (t$zinkod[2] == "25" && t$zinkod[3] == "51" && all(diff(t$NDZ_sanemsanas_datums) != 0) &&
  #           t$PS_code[1] == '______________' && t$NM_code[1] == '_____________') {
  #  # (divi atgrišanās kodi), tiek atlaists. Pieņemu, ka cilvēks nesāka darbu.
  #  yt <- y2[v, ]
  #  yt$dienas <- 0
  #} else if (t$zinkod[2] == "53" && t$zinkod[3] == "54" && diff(t$NDZ_sanemsanas_datums[2:3]) != 0 &&
  #           diff(t$NDZ_sanemsanas_datums[1:2]) == 0) {
  #  yt <- y2[v, ]
  #  yt$dienas <- as.numeric(difftime(t$last_date[3], t$NDZ_sanemsanas_datums[3], units = "days")) + 1  
  } else {stop("Starpkodi3_51: Trūkst izstrādes koda.")}
  
  return(yt) 
}

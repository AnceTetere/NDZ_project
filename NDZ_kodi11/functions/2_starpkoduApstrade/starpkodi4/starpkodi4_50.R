starpkodi4_50 <- function(y, t, prev, v) {

  if (t$zinkod[2] == "50") {
    yt <- starpkodi4_50_50(y, t, prev, v)
  } else {stop("starpkodi4_50: Trūkst izstrādes koda.")}
  
  rm(y, t, prev, v)  
  return(yt) 
}
  
#
#if (t$zinkod[2] == "51") {
#  yt <- starpkodi4_50_51(y, t, prev, v)
#} else if (t$zinkod[2] == "21" && t$zinkod[3] == "51" && t$zinkod[4] == "11" && 
#           t$NDZ_sanemsanas_datums[1] != t$NDZ_sanemsanas_datums[2] &&
#           t$NDZ_sanemsanas_datums[2] == t$NDZ_sanemsanas_datums[3] &&
#           t$NDZ_sanemsanas_datums[3] != t$NDZ_sanemsanas_datums[4]) {
#  days1 <- as.numeric(difftime(as.Date(t$NDZ_sanemsanas_datums[1]), prev, units = "days")) - 1 # jo atvaļinājums
#  days2 <- 1 #Diena, kad ierodas darbā pēc bezalgas atvaļinājuma un iesniedz atlūgumu
#  days3 <- as.numeric(difftime(as.Date(t$last_date[4]), as.Date(t$NDZ_sanemsanas_datums[4]), units = "days")) + 1 
#  days <- days1 + days2 + days3
#  rm(days1, days2, days3)
#  
#  yt <- y[v, ]
#  yt$dienas <- days
#} else if (t$zinkod[2] == "25" &&  t$zinkod[3] == "51" && t$zinkod[4] == "51" &&
#          all(diff(t$NDZ_sanemsanas_datums[2:3]) == 0) && all(sapply(c(1,3), function(i) all(diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0)))) {
#  yt <- y[v, ]
#  yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) #nav mīnus, jo ambigusous, vai atgriežoties no bezalgas atvaļinājuma viņš to dienu pirms atlaišanas nostrādāja vai nē.
#} else if (t$zinkod[2] == "21" &&  t$zinkod[3] == "51" && t$zinkod[4] == "51" &&
#           diff(t$NDZ_sanemsanas_datums[2:3]) == 0 && all(sapply(c(1,3), function(i) all(diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0)))) {
#  yt <- y[v, ]
#  yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1 # jo atvaļinājums
#} else if (t$zinkod[2] == "25" &&  t$zinkod[3] == "51" && t$zinkod[4] == "11" &&
#           diff(t$NDZ_sanemsanas_datums[2:3]) == 0 && all(sapply(c(1,3), function(i) all(diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0)))) {
#  days1 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days"))
#  days2 <- as.numeric(difftime(t$last_date[4], t$NDZ_sanemsanas_datums[4], units = "days")) + 1 # jo darbs
#  
#  yt <- y[v, ]
#  yt$dienas <- sum(days1, days2)
#  rm(days1, days2)
#
  #

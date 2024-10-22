starpkodi4 <- function(y, t, prev, v) {
  
  if (t$zinkod[1] == "11") {
    yt <- starpkodi4_11(y, t, prev, v)
#  } else if (t$zinkod[1] == "51") {
#    yt <- starpkodi4_51(y, t, prev, v)
#  } else if (t$zinkod[1] == "50") {
#    yt <- starpkodi4_50(y, t, prev, v)
# } else if (t$zinkod[1] == "53") {
#    yt <- starpkodi4_53(y, t, prev, v)
# } else if (t$zinkod[1] == "91") {
#   yt <- starpkodi4_91(y, t, prev, v)
# } else if (t$zinkod[1] == "25" && t$zinkod[2] == "51" && 
#            t$NDZ_sanemsanas_datums[1] == t$NDZ_sanemsanas_datums[2] && 
#            t$zinkod[3] == "11" && t$zinkod[4] == "25" && 
#            t$NDZ_sanemsanas_datums[3] != t$NDZ_sanemsanas_datums[4]) {
#    days <- as.numeric(difftime(as.Date(t$NDZ_sanemsanas_datums[4]), as.Date(t$NDZ_sanemsanas_datums[3]), units = "days")) 
#    yt <- y[v, ]
#    yt$dienas <- days
#  } else if (t$zinkod[1] == "92" && t$zinkod[2] == "91" &&  t$zinkod[3] == "92" && t$zinkod[4] == "25" && 
#      all(diff(t$NDZ_sanemsanas_datums) != 0)) {
#    days1 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[2], t$NDZ_sanemsanas_datums[1], units = "days")) 
#    days2 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[4], t$NDZ_sanemsanas_datums[3], units = "days")) + 1 
#
#    yt <- y[v, ]
#    yt$dienas <- sum(days1, days2)
#    rm(days1, days2)
#  } else if (t$zinkod[1] == "41" && t$zinkod[2] == "50" &&  t$zinkod[3] == "51" && t$zinkod[4] == "40" && 
#             all(diff(t$NDZ_sanemsanas_datums[1:2]) == 0) && all(diff(t$NDZ_sanemsanas_datums[2:4]) != 0)) {
#    yt <- y[v, ]
#    yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[4], t$NDZ_sanemsanas_datums[3], units = "days"))
#  } else if (t$zinkod[1] == "26" && t$zinkod[2] == "11" &&  t$zinkod[3] == "11" && t$zinkod[4] == "50" && 
#             all(diff(t$NDZ_sanemsanas_datums[1:2]) == 0) && all(diff(t$NDZ_sanemsanas_datums[2:4]) != 0)) {
#    yt <- y[v, ]
#    yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[4], t$NDZ_sanemsanas_datums[3], units = "days"))
#  } else if (t$zinkod[1] == "92" && t$zinkod[2] == "91" &&  t$zinkod[3] == "92" && t$zinkod[4] == "50" && 
#             all(diff(t$NDZ_sanemsanas_datums[3:4]) == 0) && all(diff(t$NDZ_sanemsanas_datums[1:3]) != 0)) {
#    yt <- y[v, ]
#    yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[2], t$NDZ_sanemsanas_datums[1], units = "days"))
#  } else if (t$zinkod[1] == "92" && t$zinkod[2] == "91" &&  t$zinkod[3] == "25" && t$zinkod[4] == "92" && 
#             all(diff(t$NDZ_sanemsanas_datums[3:4]) == 0) && all(diff(t$NDZ_sanemsanas_datums[1:3]) != 0)) {
#    yt <- y[v, ]
#    yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[2], t$NDZ_sanemsanas_datums[1], units = "days"))
#  } else if (t$zinkod[1] == "92" && t$zinkod[2] == "91" &&  t$zinkod[3] == "25" && t$zinkod[4] == "92" && 
#             all(diff(t$NDZ_sanemsanas_datums[3:4]) == 0) && all(diff(t$NDZ_sanemsanas_datums[1:3]) != 0)) {
#    yt <- y[v, ]
#    yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[2], t$NDZ_sanemsanas_datums[1], units = "days"))
#  } else if (t$zinkod[1] == "25" && t$zinkod[2] == "11" &&  t$zinkod[3] == "50" && t$zinkod[4] == "51" && all(diff(t$NDZ_sanemsanas_datums) != 0)) {
#    days1 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days"))
#    days2 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[3], t$NDZ_sanemsanas_datums[2], units = "days"))
#    days3 <- as.numeric(difftime(t$last_date[4], t$NDZ_sanemsanas_datums[4], units = "days")) + 1
#    
#    yt <- y[v, ]
#    yt$dienas <- sum(days1, days2, days3)
#    rm(days1, days2, days3)
#  } else if (t$zinkod[1] == "41" && t$zinkod[2] == "50" &&  t$zinkod[3] == "51" && t$zinkod[4] == "50") {
#    yt <- y[v, ]
#    yt$dienas <- sum(sapply(seq(1,4,by=2), function(i) as.numeric(difftime(t$NDZ_sanemsanas_datums[i+1], t$NDZ_sanemsanas_datums[i], units = "days"))))
  } else {stop("Starpkodi4 iztrūkst apstrādes koda.")}
  
  if(is.na(yt$PS_code[1])) {stop("Dienas NA.")}
  yt$zinkod <- "combined"  #jo starpkodu dienu sarēķins
  return(yt)
}

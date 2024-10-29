starpkodi7 <- function(y, t, prev, v) {
  
  #if (t$zinkod[1] == "11") {
  #  yt <- starpkodi7_11(y, t, prev, v)
  #} else if (all(t$zinkod[c(1,4,7)] %in% c("41", "51", "54", "92")) && 
  #           all(t$zinkod[c(2,3,5)] %in% c("40", "50", "53", "91")) && 
  #           t$zinkod[6] %in% c("21", "22", "23", "25", "24", "29") && 
  #           all(diff(t$NDZ_sanemsanas_datums[2:6]) != 0) && 
  #           all(sapply(c(1,6), function(i) all(diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0)))) {
  #          yt <- y[v, ]
  #          yt$dienas <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[2], prev, units = "days")) - 1, 
  #                           as.numeric(difftime(t$NDZ_sanemsanas_datums[3], t$NDZ_sanemsanas_datums[1], units = "days")), 
  #                           as.numeric(diff(t$NDZ_sanemsanas_datums[4:5])),
  #                           as.numeric(diff(t$NDZ_sanemsanas_datums[6:7])) + 1) 
  #  
  #} else if (all(t$zinkod[c(1,3,5)] %in% c("41", "51", "54", "92")) && 
  #           all(t$zinkod[c(2,4,6)] %in% c("40", "50", "53", "91")) && 
  #           t$zinkod[7] %in% c("21", "22", "23", "25", "24", "29") && 
  #           all(diff(t$NDZ_sanemsanas_datums) != 0)) {
  #           yt <- y[v, ]
  #           yt$dienas <- sum(sapply(seq(1,6,by=2), function(i) as.numeric(diff(t$NDZ_sanemsanas_datums[i:(i+1)]))))
  #} else 
    if (all(t$zinkod[c(1,4,6,7)] %in% c("41", "51", "54", "92")) && 
             all(t$zinkod[c(2,3)] %in% c("40", "50", "53", "91")) && 
             t$zinkod[5] %in% c("21", "22", "23", "25", "24", "29")) {
          if (all(sapply(c(1,2,4,5,6), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0)) && 
              diff(t$NDZ_sanemsanas_datums[3:4]) == 0) {
            if (t$PS_code[1] == '___________' && t$NM_code[1] == '___________') {
              yt <- y[v, ]
              yt$dienas <- sum(as.numeric(diff(t$NDZ_sanemsanas_datums[1:2])),
                               as.numeric(difftime(t$NDZ_sanemsanas_datums[5], t$NDZ_sanemsanas_datums[3], units = "days")) + 1)
            } else {stop("Starpkodi7 iztrūkst apstrādes koda.")}
          } else {stop("Starpkodi7 iztrūkst apstrādes koda.")}
    } else if (all(t$zinkod[c(1,3,4,6)] %in% c("41", "51", "54", "92")) && 
               all(t$zinkod[c(2,5,7)] %in% c("40", "50", "53", "91"))) {
           if (all(sapply(c(1,3,4,5,6), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0)) && 
               diff(t$NDZ_sanemsanas_datums[2:3]) == 0) {
          yt <- y[v, ]
          yt$dienas <- sum(as.numeric(diff(t$NDZ_sanemsanas_datums[1:2])), 
                           as.numeric(difftime(t$NDZ_sanemsanas_datums[5], t$NDZ_sanemsanas_datums[4], units = "days")) + 1, 
                           as.numeric(diff(t$NDZ_sanemsanas_datums[6:7])))
      } else {stop("Starpkodi7 iztrūkst apstrādes koda.")}
  } else {stop("Starpkodi7 iztrūkst apstrādes koda.")}
  
  if(is.na(yt$PS_code[1])) {stop("Dienas NA.")}
  yt$zinkod <- "combined"  #jo starpkodu dienu sarēķins

  rm(y, t, prev, v)
  return(yt)
}
#} else if (all(t$zinkod[c(1,3,4,6)] %in% c("91","40")) && all(t$zinkod[c(2,5,7)] %in% c("92","41")) &&
#           all(sapply(seq(1,6,by=2), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0)) &&
#           all(sapply(seq(4,7,by=2), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0))) {

#  days1 <- as.numeric(difftime(as.Date(t$NDZ_sanemsanas_datums[1]), prev, units = "days")) - 1 
#  days2 <- as.numeric(difftime(as.Date(t$NDZ_sanemsanas_datums[3]), as.Date(t$NDZ_sanemsanas_datums[2]), units = "days"))
#  days3 <- as.numeric(difftime(as.Date(t$NDZ_sanemsanas_datums[4]), as.Date(t$NDZ_sanemsanas_datums[5]), units = "days")) 
#  days4 <- as.numeric(difftime(as.Date(t$NDZ_sanemsanas_datums[6]), as.Date(t$NDZ_sanemsanas_datums[7]), units = "days"))

#  yt <- y[v, ]
#  yt$dienas <- sum(days1, days2, days3, days4)
#  rm(days1, days2, days3, days4)
#} else if (t$zinkod[1] == "50" && 
#           t$zinkod[2] == "51" && 
#           t$zinkod[3] == "50" && 
#           t$zinkod[4] == "51" && 
#           t$zinkod[5] == "50" && 
#           t$zinkod[6] == "25" && 
#           t$zinkod[7] == "51" && 
#           t$NDZ_sanemsanas_datums[6] == t$NDZ_sanemsanas_datums[7] &&
#           all(!diff(t$NDZ_sanemsanas_datums[1:6]) == 0)) {

#  days1 <- as.numeric(difftime(as.Date(t$NDZ_sanemsanas_datums[1]), prev, units = "days")) - 1 
#  days2 <- as.numeric(difftime(as.Date(t$NDZ_sanemsanas_datums[3]), as.Date(t$NDZ_sanemsanas_datums[2]), units = "days"))
#  days3 <- as.numeric(difftime(as.Date(t$NDZ_sanemsanas_datums[5]), as.Date(t$NDZ_sanemsanas_datums[4]), units = "days")) 
#  days <- days1 + days2 + days3
#  rm(days1, days2, days3)

# yt <- y[v, ]
#  yt$dienas <- days
#} else if (t$zinkod[1] == "50" && 
#           t$zinkod[2] == "51" && 
#           t$zinkod[3] == "50" && 
#           t$zinkod[4] == "51" && 
#           t$zinkod[5] == "50" && 
#           t$zinkod[6] == "51" && 
#           t$zinkod[7] %in% c("21", "25") && 
#           all(!diff(t$NDZ_sanemsanas_datums) == 0)) {
#  days1 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1 
#  days2 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[3], t$NDZ_sanemsanas_datums[2], units = "days"))
#  days3 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[5], t$NDZ_sanemsanas_datums[4], units = "days")) 
#  days4 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[7], t$NDZ_sanemsanas_datums[6], units = "days")) + 1  
#  days <- days1 + days2 + days3 + days4
#  rm(days1, days2, days3, days4)

#  yt <- y[v, ]
#  yt$dienas <- days
#} else if (t$zinkod[1] == "91" && 
#           t$zinkod[2] == "92" && 
#           t$zinkod[3] == "91" && 
#           t$zinkod[4] == "92" && 
#           t$zinkod[5] == "91" && 
#           t$zinkod[6] == "92" && 
#           t$zinkod[7] %in% c("21", "25") && 
#           all(!diff(t$NDZ_sanemsanas_datums) == 0)) {
#  days1 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1 
#  days2 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[3], t$NDZ_sanemsanas_datums[2], units = "days"))
#  days3 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[5], t$NDZ_sanemsanas_datums[4], units = "days")) 
#  days4 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[7], t$NDZ_sanemsanas_datums[6], units = "days")) + 1  

#  yt <- y[v, ]
#  yt$dienas <- sum(days1, days2, days3, days4) 
#  rm(days1, days2, days3, days4)
#} else if (t$zinkod[1] == "50" && 
#           t$zinkod[2] == "51" && 
#           t$zinkod[3] == "25" && 
#           t$zinkod[4] == "11" && 
#           t$zinkod[5] == "50" && 
#           t$zinkod[6] == "25" && 
#           t$zinkod[7] == "51" && 
#           all(diff(t$NDZ_sanemsanas_datums[1:4]) != 0) &&
#           all(diff(t$NDZ_sanemsanas_datums[4:5]) == 0) &&
#           all(diff(t$NDZ_sanemsanas_datums[6:7]) == 0)) {
#  days1 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1 
#  days2 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[3], t$NDZ_sanemsanas_datums[2], units = "days")) + 1 

#  yt <- y[v, ]
#  yt$dienas <- sum(days1, days2)
#  rm(days1, days2)
#} else if (t$zinkod[1] == "91" && 
#           t$zinkod[2] == "92" && 
#           t$zinkod[3] == "91" && 
#           t$zinkod[4] == "92" && 
#           t$zinkod[5] == "91" && 
#           t$zinkod[6] == "25" && 
#           t$zinkod[7] == "92" && 
#           all(diff(t$NDZ_sanemsanas_datums[1:6]) != 0) &&
#           all(diff(t$NDZ_sanemsanas_datums[6:7]) == 0)) {
#  days1 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1 
#  days2 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[3], t$NDZ_sanemsanas_datums[2], units = "days")) + 1
#  days3 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[5], t$NDZ_sanemsanas_datums[4], units = "days")) + 1 

# yt <- y[v, ]
#  yt$dienas <- sum(days1, days2, days3)
# rm(days1, days2, days3)

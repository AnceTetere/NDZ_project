starpkodi7 <- function(y, t, prev, v) {
  
  if (t$zk[1] %in% c("41", "51", "54", "92")) {
    yt <- starpkodi7_51(y, t, prev, v)
  } else if (t$zk[1] %in% c("11", "14", "16", "61")) {
    yt <- starpkodi7_11(y, t, prev, v)
  } else {stop("Starpkodi4 iztrūkst apstrādes koda.")}
  
  if(is.na(yt$PS_code[1])) {stop("Dienas NA.")}
  yt$zk <- "combined" 
  
  rm(y, t, prev, v)
  return(yt)
}

#} else if (all(t$zk[c(1,3,4,6)] %in% c("91","40")) && all(t$zk[c(2,5,7)] %in% c("92","41")) &&
#           all(sapply(seq(1,6,by=2), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0)) &&
#           all(sapply(seq(4,7,by=2), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0))) {

#  days1 <- as.numeric(difftime(as.Date(t$NDZ_sanemsanas_datums[1]), prev, units = "days")) - 1  
#  days2 <- as.numeric(difftime(as.Date(t$NDZ_sanemsanas_datums[3]), as.Date(t$NDZ_sanemsanas_datums[2]), units = "days"))
#  days3 <- as.numeric(difftime(as.Date(t$NDZ_sanemsanas_datums[4]), as.Date(t$NDZ_sanemsanas_datums[5]), units = "days")) # te ta nav ķļuda, vien sākuma un beigu kodi samainījusies vietām, jo vienāds datums
#  days4 <- as.numeric(difftime(as.Date(t$NDZ_sanemsanas_datums[6]), as.Date(t$NDZ_sanemsanas_datums[7]), units = "days"))

#  yt <- y[v, ]
#  yt$dienas <- sum(days1, days2, days3, days4)
#  rm(days1, days2, days3, days4)
#} else if (t$zk[1] == "50" && 
#           t$zk[2] == "51" && 
#           t$zk[3] == "50" && 
#           t$zk[4] == "51" && 
#           t$zk[5] == "50" && 
#           t$zk[6] == "25" && 
#           t$zk[7] == "51" && 
#           t$NDZ_sanemsanas_datums[6] == t$NDZ_sanemsanas_datums[7] &&
#           all(!diff(t$NDZ_sanemsanas_datums[1:6]) == 0)) {

#  days1 <- as.numeric(difftime(as.Date(t$NDZ_sanemsanas_datums[1]), prev, units = "days")) - 1  
#  days2 <- as.numeric(difftime(as.Date(t$NDZ_sanemsanas_datums[3]), as.Date(t$NDZ_sanemsanas_datums[2]), units = "days"))
#  days3 <- as.numeric(difftime(as.Date(t$NDZ_sanemsanas_datums[5]), as.Date(t$NDZ_sanemsanas_datums[4]), units = "days")) 

#  days <- days1 + days2 + days3
#  rm(days1, days2, days3)

# yt <- y[v, ]
#  yt$dienas <- days
#} else if (t$zk[1] == "50" && 
#           t$zk[2] == "51" && 
#           t$zk[3] == "50" && 
#           t$zk[4] == "51" && 
#           t$zk[5] == "50" && 
#           t$zk[6] == "51" && 
#           t$zk[7] %in% c("21", "25") && 
#           all(!diff(t$NDZ_sanemsanas_datums) == 0)) {
#  days1 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1 
#  days2 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[3], t$NDZ_sanemsanas_datums[2], units = "days"))
#  days3 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[5], t$NDZ_sanemsanas_datums[4], units = "days")) 
#  days4 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[7], t$NDZ_sanemsanas_datums[6], units = "days")) + 1  
#  days <- days1 + days2 + days3 + days4
#  rm(days1, days2, days3, days4)

#  yt <- y[v, ]
#  yt$dienas <- days
#} else if (t$zk[1] == "91" && 
#           t$zk[2] == "92" && 
#           t$zk[3] == "91" && 
#           t$zk[4] == "92" && 
#           t$zk[5] == "91" && 
#           t$zk[6] == "92" && 
#           t$zk[7] %in% c("21", "25") && 
#           all(!diff(t$NDZ_sanemsanas_datums) == 0)) {
#  days1 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1 
#  days2 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[3], t$NDZ_sanemsanas_datums[2], units = "days"))
#  days3 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[5], t$NDZ_sanemsanas_datums[4], units = "days")) 
#  days4 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[7], t$NDZ_sanemsanas_datums[6], units = "days")) + 1 #jo atlaišana 

#  yt <- y[v, ]
#  yt$dienas <- sum(days1, days2, days3, days4) 
#  rm(days1, days2, days3, days4)
#} else if (t$zk[1] == "50" && 
#           t$zk[2] == "51" && 
#           t$zk[3] == "25" && 
#           t$zk[4] == "11" && 
#           t$zk[5] == "50" && 
#           t$zk[6] == "25" && 
#           t$zk[7] == "51" && 
#           all(diff(t$NDZ_sanemsanas_datums[1:4]) != 0) &&
#           all(diff(t$NDZ_sanemsanas_datums[4:5]) == 0) &&
#           all(diff(t$NDZ_sanemsanas_datums[6:7]) == 0)) {
#  days1 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1 
#  days2 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[3], t$NDZ_sanemsanas_datums[2], units = "days")) + 1 

#  yt <- y[v, ]
#  yt$dienas <- sum(days1, days2)
#  rm(days1, days2)
#} else if (t$zk[1] == "91" && 
#           t$zk[2] == "92" && 
#           t$zk[3] == "91" && 
#           t$zk[4] == "92" && 
#           t$zk[5] == "91" && 
#           t$zk[6] == "25" && 
#           t$zk[7] == "92" && 
#           all(diff(t$NDZ_sanemsanas_datums[1:6]) != 0) &&
#           all(diff(t$NDZ_sanemsanas_datums[6:7]) == 0)) {
#  days1 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1 
#  days2 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[3], t$NDZ_sanemsanas_datums[2], units = "days")) + 1
#  days3 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[5], t$NDZ_sanemsanas_datums[4], units = "days")) + 1 # viena diena, kad tiek atlaists

# yt <- y[v, ]
#  yt$dienas <- sum(days1, days2, days3)
# rm(days1, days2, days3)

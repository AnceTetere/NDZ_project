starpkodi4_50 <- function(y, t, prev, v) {

  if (t$zinkod[2] %in% c("40", "50", "53", "91")) {
    yt <- starpkodi4_50_50(y, t, prev, v)
  } else if (t$zinkod[2] %in% c("41", "51", "54", "92")) {
    yt <- starpkodi4_50_51(y, t, prev, v)
  } else if (t$zinkod[2] %in% c("21", "22", "23", "24", "25", "29")) {
    yt <- starpkodi4_50_25(y, t, prev, v)
  } else {stop("starpkodi4_50: Trūkst izstrādes koda.")}
  
  rm(y, t, prev, v)  
  return(yt) 
}

#starpkodi4_53 <- function(y2, t, prev, v) {
#  
#  if (t$zinkod[2] == "54" && t$zinkod[3] == "53" && t$zinkod[4] == "21" && 
#      all(!diff(t$NDZ_sanemsanas_datums) == 0)) {
#    days1 <- as.numeric(difftime(as.Date(t$beidz[1]), prev, units = "days")) - 1  
#    days2 <- as.numeric(difftime(as.Date(t$beidz[3]), as.Date(t$sak[2]), units = "days")) 
#    
#    yt <- y2[v, ]
#    yt$dienas <- sum(days1, days2)
#    rm(days1, days2)
#  } else if (t$zinkod[2] == "54" && t$zinkod[3] == "53" && t$zinkod[4] == "25" && 
#             all(!diff(t$NDZ_sanemsanas_datums) == 0)) {
#    days1 <- as.numeric(difftime(as.Date(t$beidz[1]), prev, units = "days")) - 1  
#    days2 <- as.numeric(difftime(as.Date(t$beidz[3]), as.Date(t$sak[2]), units = "days")) 
#    
#    yt <- y2[v, ]
#    yt$dienas <- sum(days1, days2)
#    rm(days1, days2)
#  } else if (t$zinkod[2] == "54" && t$zinkod[3] == "25" && t$zinkod[4] == "53" && 
#             all(!diff(t$NDZ_sanemsanas_datums[1:3]) == 0) && t$NDZ_sanemsanas_datums[3] == t$NDZ_sanemsanas_datums[4]) {
#    days1 <- as.numeric(difftime(as.Date(t$beidz[1]), prev, units = "days"))
#    days2 <- as.numeric(difftime(as.Date(t$beidz[3]), as.Date(t$sak[2]), units = "days"))
#    
#    yt <- y2[v, ]
#    yt$dienas <- sum(days1, days2)
#    rm(days1, days2)
#  } else if (t$zinkod[2] == "54" && t$zinkod[3] == "21" && t$zinkod[4] == "53" && 
#             all(diff(t$NDZ_sanemsanas_datums[1:3]) != 0) && t$NDZ_sanemsanas_datums[3] == t$NDZ_sanemsanas_datums[4]) {
#    days1 <- as.numeric(difftime(as.Date(t$beidz[1]), prev, units = "days")) - 1 
#    days2 <- as.numeric(difftime(as.Date(t$beidz[3]), as.Date(t$sak[2]), units = "days"))
#    
#    yt <- y2[v, ]
#    yt$dienas <- sum(days1, days2)
#    rm(days1, days2)
#  } else if (t$zinkod[2] == "53" && t$zinkod[3] == "54" && t$zinkod[4] == "25" && 
#             all(diff(t$NDZ_sanemsanas_datums) != 0)) {
#    days1 <- as.numeric(difftime(as.Date(t$beidz[2]), prev, units = "days")) - 1 
#    days2 <- as.numeric(difftime(as.Date(t$beidz[4]), as.Date(t$sak[3]), units = "days")) + 1 
#    
#    yt <- y2[v, ]
#    yt$dienas <- sum(days1, days2)
#    rm(days1, days2)
#  } else {
#    stop("Starpkodi4_53 iztrūkst apstrādes koda.")
#  }
#  
#  if(is.na(yt$PS_code[1])) {stop("Dienas NA.")}
#  yt$zinkod <- "combined"  #jo starpkodu dienu sarēķins
#  return(yt)
#}

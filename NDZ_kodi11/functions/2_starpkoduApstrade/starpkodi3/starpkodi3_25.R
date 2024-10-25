starpkodi3_25 <- function(y, t, prev, v) {

  if (t$zinkod[2] == "51") {
    yt <- starpkodi3_25_51(y, t, prev, v)
  } else {stop("Starpkodi3_25: Trūkst izstrādes koda.")}
  
  
  
    
    
    
#    if (t$zinkod[2] == "50" && t$zinkod[3] == "51" && all(diff(t$NDZ_sanemsanas_datums) == 0)) {
#    yt <- y[v, ]
#    yt$dienas <- as.numeric(difftime(t$beidz[1], prev, units = "days")) - 1 
#    } else if (t$zinkod[2] == "50" && t$zinkod[3] == "51" && diff(t$NDZ_sanemsanas_datums[2:3]) != 0 && diff(t$NDZ_sanemsanas_datums[1:2]) == 0) {
#      yt <- y[v, ]
#      yt$dienas <- as.numeric(difftime(t$beidz[1], prev, units = "days")) - 1 
#    } else {stop("Starpkodi3_25: Trūkst izstrādes koda.")}
#    
#  
#  
#  
#    if (t$zinkod[2] == "41" && t$zinkod[3] == "11" && 
#             all(diff(t$NDZ_sanemsanas_datums[1:2]) == 0) && 
#             all(diff(t$NDZ_sanemsanas_datums[2:3]) != 0)) {
#    yt <- y[v, ]
#    yt$dienas <- as.numeric(difftime(t$last_date[3], t$sak_darbu[3], units = "days")) + 1 #jo darbs
#    } else } else if (t$zinkod[2] %in% c("92","41") && t$zinkod[3] %in% c("92","41") && all(diff(t$NDZ_sanemsanas_datums) == 0)) {
#      yt <- y[v, ]
#      yt$dienas <- 0
#    } else if (t$zinkod[2] == "41" && t$zinkod[3] == "11" && all(diff(t$NDZ_sanemsanas_datums) != 0)) {
#      yt <- y[v:(v+1), ]
#      yt <- yt[yt$zinkod == "11", ]
#    } else if (t$zinkod[2] == "41" && t$zinkod[3] == "41" && diff(t$NDZ_sanemsanas_datums[1:2]) == 0 &&
#               diff(t$NDZ_sanemsanas_datums[2:3]) != 0) {
#      yt <- y[v:(v+1), ]
#      yt <- yt[yt$zinkod == "11", ]
#    } else {stop("Starpkodi3_25: Trūkst izstrādes koda.")}
#  
#  
#  
#  
#    
#  if (t$zinkod[2] == "11" && t$zinkod[3] == "50" && all(diff(t$NDZ_sanemsanas_datums) != 0)) {
#    days1 <- as.numeric(difftime(t$beidz[1], prev, units = "days"))
#    days2 <- as.numeric(difftime(t$beidz[3], t$sak_darbu[2], units = "days"))
#    
#    yt <- y[v, ]
#    yt$dienas <- sum(days1, days2)
#    rm(days1, days2)
#  } else if (t$zinkod[2] == "11" && t$zinkod[3] == "50" && diff(t$NDZ_sanemsanas_datums[2:3]) == 0 &&
#             diff(t$NDZ_sanemsanas_datums[1:2]) != 0) {
#    yt <- y[v, ]
#    yt$dienas <- as.numeric(difftime(t$beidz[1], prev, units = "days"))
#  } else {stop("Starpkodi3_25: Trūkst izstrādes koda.")}

  rm(y, t, prev, v)
  return(yt) 
}

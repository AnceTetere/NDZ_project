starpkodi4_50_51 <- function(y, t, prev, v) {
  
  if (t$zinkod[3] == "50") {
    yt <- starpkodi4_50_51_50(y, t, prev, v)
  } else if (t$zinkod[3] %in% c("21", "25")) {
    yt <- starpkodi4_50_51_25(y, t, prev, v)
  } else {stop("starpkodi4_50_51: Trūkst izstrādes koda.")}
  
    
    
     
  #} else if (t$zinkod[3] == "51" && t$zinkod[4] == "21" &&
             #            all(sapply(c(1,3), function(i) all(diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) && 
             #            diff(t$NDZ_sanemsanas_datums[2:3]) == 0) {
             #   days1 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1 
             #   days2 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[4], t$NDZ_sanemsanas_datums[3], units = "days")) + 1 
             #   
             #   yt <- y[v, ]
             #   yt$dienas <- sum(days1, days2)
             #   rm(days1, days2)
             
  
   
 # } else if (t$zinkod[3] == "53" && t$zinkod[4] == "54" && all(diff(t$NDZ_sanemsanas_datums) != 0)) {
 #   days1 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1  
 #   days2 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[3], t$NDZ_sanemsanas_datums[2], units = "days"))
 #   days3 <- as.numeric(difftime(t$last_date[4], t$NDZ_sanemsanas_datums[4], units = "days")) + 1 
 #   
 #   yt <- y[v, ]
 #   yt$dienas <- sum(days1, days2, days3)
 #   rm(days1, days2, days3)
 # } else  if (t$zinkod[3] == "91" && t$zinkod[4] == "92" && all(diff(t$NDZ_sanemsanas_datums) != 0)) {
 #   days1 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1  
 #   days2 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[3], t$NDZ_sanemsanas_datums[2], units = "days"))
 #   days3 <- as.numeric(difftime(t$last_date[4], t$NDZ_sanemsanas_datums[4], units = "days")) + 1 
 #   
 #   yt <- y[v, ]
 #   yt$dienas <- sum(days1, days2, days3)
 #   rm(days1, days2, days3)
 # } else 
  
   # } else if (t$zinkod[3] == "51" && t$zinkod[4] == "91" && 
 #            all(diff(t$NDZ_sanemsanas_datums[1:2]) == 0) && 
 #            all(diff(t$NDZ_sanemsanas_datums[2:4]) != 0)) {
 #   days1 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1 
 #   days2 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[4], t$NDZ_sanemsanas_datums[3], units = "days"))
 #   
 #   yt <- y[v, ]
 #   yt$dienas <- sum(days1, days2)
 #   rm(days1, days2)
 #
 # } else if (t$zinkod[3] == "51" && t$zinkod[4] == "21" && diff(t$NDZ_sanemsanas_datums[1:2]) == 0) {
 #   yt <- y[v, ]
 #   yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[4], t$NDZ_sanemsanas_datums[3], units = "days")) + 1 
 # } else if (t$zinkod[3] == "51" && t$zinkod[4] %in% c("21","25") && all(diff(t$NDZ_sanemsanas_datums) != 0)) {
 #   days1 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1
 #   days2 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[4], t$NDZ_sanemsanas_datums[3], units = "days")) + 1
 #   
 #   yt <- y[v, ]
 #   yt$dienas <- sum(days1, days2)
 #   rm(days1, days2)
#  } else {stop("starpkodi4_50_51: Trūkst izstrādes koda.")}
  
  rm(y, t, prev, v)
  return(yt) 
}


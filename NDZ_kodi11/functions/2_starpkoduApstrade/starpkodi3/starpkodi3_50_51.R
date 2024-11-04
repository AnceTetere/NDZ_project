starpkodi3_50_51 <- function(y, t, prev, v) {

  if (t$zinkod[3] %in% c("21", "25")) {
       if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
         yt <- y[v, ]
         yt$dienas <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1,  
                          as.numeric(diff(t$NDZ_sanemsanas_datums[2:3])))
       } else if (diff(t$NDZ_sanemsanas_datums[1:2]) == 0 && diff(t$NDZ_sanemsanas_datums[2:3]) != 0) {
         yt <- y[v:(v+1), ]
         yt <- yt[yt$zinkod == "11", ]
       } else {stop("starpkodi3_50_51: Iztrūkst apstrādes koda.")}
    
  } else if (t$zinkod[3] == "21") {
        if (diff(t$NDZ_sanemsanas_datums[1:2]) == 0 && diff(t$NDZ_sanemsanas_datums[2:3]) != 0 &&
            t$PS_code[1] == '__________' && t$NM_code[1] == '__________') {
          yt <- y[v, ]
          yt$dienas <- 0
          #  days1 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1 
          #  days2 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[3], t$NDZ_sanemsanas_datums[2], units = "days"))
          
          #  yt <- y[v, ]
          #  yt$dienas <- sum(days1, days2)
          #  rm(days1, days2)
        } else {stop("starpkodi3_50_51: Iztrūkst apstrādes koda.")}
  
  
    
    
  #} else 
  #} else if (t$zinkod[3] == "91" && all(!diff(t$NDZ_sanemsanas_datums) == 0)) {
  #  days1 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1 
  #  days2 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[3], t$NDZ_sanemsanas_datums[2], units = "days")) + 1 

#    yt <- y[v, ]
#    yt$dienas <- sum(days1, days2)
#    rm(days1, days2)
    
  #} else if (t$zinkod[3] == "22" && all(diff(t$NDZ_sanemsanas_datums) != 0)) {
  #  days1 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1 
  #  days2 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[3], t$NDZ_sanemsanas_datums[2], units = "days")) + 1 
    
   # yt <- y[v, ]
  #  yt$dienas <- sum(days1, days2)
   # rm(days1, days2)
  #} else if (t$zinkod[3] == "53" && diff(t$NDZ_sanemsanas_datums[1:2]) != 0 && diff(t$NDZ_sanemsanas_datums[2:3]) == 0) {
   # yt <- y[v, ]
  #  yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1
  #} else if (t$zinkod[3] == "53" && all(diff(t$NDZ_sanemsanas_datums) != 0)) {
   # days1 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1
  #  days2 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[3], t$NDZ_sanemsanas_datums[2], units = "days"))
    
   # yt <- y[v, ]
  #  yt$dienas <- sum(days1, days2)
   # rm(days1, days2)
  } else {stop("starpkodi3_50_51: Iztrūkst apstrādes koda.")}
  
  return(yt) 
}

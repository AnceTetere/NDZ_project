starpkodi3_91_92 <- function(y, t, prev, v) {
  
  if (t$zinkod[3] %in% c("21", "25")) {
        if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
          yt <- y2[v, ]
          yt$dienas <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")), as.numeric(diff(t$NDZ_sanemsanas_datums[2:3]))) 
        } else {stop("Starpkodi3_91_92: Trūkst izstrādes koda.")}
  } else if (t$zinkod[3] == "50") {
        if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
          yt <- y2[v, ]
          yt$dienas <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1, 
                           as.numeric(diff(t$NDZ_sanemsanas_datums[2:3])))
        } else if (diff(t$NDZ_sanemsanas_datums[1:2]) != 0 && diff(t$NDZ_sanemsanas_datums[2:3]) == 0) {
          yt <- y2[v, ]
          yt$dienas <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1, 
                           as.numeric(diff(t$NDZ_sanemsanas_datums[2:3])))
        } else {stop("Starpkodi3_91_92: Trūkst izstrādes koda.")}
  
      
    
#  } else if (t$zinkod[3] == "50") {
#    if (diff(t$NDZ_sanemsanas_datums[1:2]) == 0 &&
#        diff(t$NDZ_sanemsanas_datums[2:3]) == 0)) { 
#      yt <- y2[v, ]
#      yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1 
#    }
#  } else if (t$zinkod[3] == "21") {}
#  if(all(diff(t$NDZ_sanemsanas_datums) != 0)) {
#    yt <- y2[v, ]
#    yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[3], t$NDZ_sanemsanas_datums[2], units = "days"))
#  }
#}
  #    #} else if (t$zinkod[3] == "40" && all(diff(t$NDZ_sanemsanas_datums) != 0)) {
  #days1 <- as.numeric(difftime(as.Date(t$NDZ_sanemsanas_datums[1]), prev, units = "days")) - 1 
  #days2 <- as.numeric(difftime(as.Date(t$NDZ_sanemsanas_datumsz[3]), as.Date(t$NDZ_sanemsanas_datums[2]), units = "days")) 
  #days <- days1 + days2 
  #rm(days1, days2)
  
  #yt <- y2[v, ]
  #yt$dienas <- days
  } else {stop("Starpkodi3_91_92: Trūkst izstrādes koda.")}  
  
return(yt)
}

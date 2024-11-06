starpkodi3_50 <- function(y, t, prev, v) {
  
  if (t$zinkod[2] %in% c("21", "22", "23", "24", "25", "29")) {
    yt <- starpkodi3_50_25(y, t, prev, v)
  } else if(t$zinkod[2] %in% c("41", "51", "54", "92")) {
       yt <- starpkodi3_50_51(y, t, prev, v)
  } else if (t$zinkod[2] %in% c("40", "50", "53", "91")) {
    yt <- starpkodi3_50_50(y, t, prev, v)
  } else if (t$zinkod[2] == "91") {
    yt <- starpkodi3_50_91(y, t, prev,v)
  } else {stop("starpkodi3_50: Iztrūkst apstrādes koda.")}  
  
  rm(y, t, prev, v)
  return(yt) 
}
#
#  
#
#    
#  #} else if(t$zinkod[2] == "11") {
#  #  yt <- starpkodi3_50_11(y, t, prev, v)
#  #} else if (t$zinkod[2] %in% c("21","24") && t$zinkod[3] == "51" && 
#  #           t$NDZ_sanemsanas_datums[2] == t$NDZ_sanemsanas_datums[3]){
#  #  yt <- y[v, ]
#  #  yt$dienas <- as.numeric(difftime(t$sak_beidz[1], prev, units = "days"))
#  #} 
#  #} else if (t$zinkod[2] == "22" && t$zinkod[3] == "51" && all(diff(t$NDZ_sanemsanas_datums[1:2]) != 0) && all(diff(t$NDZ_sanemsanas_datums[2:3]) == 0)) {
#  #  yt <- y[v, ]
#  #  yt$dienas <- as.numeric(difftime(t$sak_beidz[1], prev, units = "days")) - 1
#
#
#starpkodi3_40 <- function(y2, t, prev, v) {
#  
#  if (t$zinkod[2] == "21" && t$zinkod[3] == "41" && diff(t$NDZ_sanemsanas_datums[1:2]) != 0 &&
#      diff(t$NDZ_sanemsanas_datums[2:3]) == 0) {
#    yt <- y2[v, ]
#    yt$dienas <- as.numeric(difftime(t$beidz[1], prev, units = "days")) - 1 
#  } else if (t$zinkod[2] == "25" && t$zinkod[3] == "41" && 
#             diff(t$NDZ_sanemsanas_datums[1:2]) != 0 && diff(t$NDZ_sanemsanas_datums[2:3]) == 0) {
#    yt <- y2[v, ]
#    yt$dienas <- as.numeric(difftime(t$beidz[1], prev, units = "days")) - 1 
#  
#  if(is.na(yt$PS_code[1])) {stop("Dienas NA.")}
#  return(yt) 
#}
#
#starpkodi3_91_92 <- function(y, t, prev, v) {
#  
#  if (t$zinkod[3] %in% c("21", "25")) {
#    if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
#      yt <- y2[v, ]
#      yt$dienas <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")), as.numeric(diff(t$NDZ_sanemsanas_datums[2:3]))) 
#    } else {stop("Starpkodi3_91_92: Trūkst izstrādes koda.")}
#  } else if (t$zinkod[3] == "50") {
#    if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
#      yt <- y2[v, ]
#      yt$dienas <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1, 
#                       as.numeric(diff(t$NDZ_sanemsanas_datums[2:3])))
#    } else if (diff(t$NDZ_sanemsanas_datums[1:2]) != 0 && diff(t$NDZ_sanemsanas_datums[2:3]) == 0) {
#      yt <- y2[v, ]
#      yt$dienas <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1, 
#                       as.numeric(diff(t$NDZ_sanemsanas_datums[2:3])))
#    } else {stop("Starpkodi3_91_92: Trūkst izstrādes koda.")}
#    
#    
#    
#    #  } else if (t$zinkod[3] == "50") {
#    #    if (diff(t$NDZ_sanemsanas_datums[1:2]) == 0 &&
#    #        diff(t$NDZ_sanemsanas_datums[2:3]) == 0)) { 
#    #      yt <- y2[v, ]
#    #      yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1 
#    #    }
#    #  } else if (t$zinkod[3] == "21") {}
#    #  if(all(diff(t$NDZ_sanemsanas_datums) != 0)) {
#    #    yt <- y2[v, ]
#    #    yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[3], t$NDZ_sanemsanas_datums[2], units = "days"))
#    #  }
#    #}
#    #    #} else if (t$zinkod[3] == "40" && all(diff(t$NDZ_sanemsanas_datums) != 0)) {
#    #days1 <- as.numeric(difftime(as.Date(t$NDZ_sanemsanas_datums[1]), prev, units = "days")) - 1 
#    #days2 <- as.numeric(difftime(as.Date(t$NDZ_sanemsanas_datumsz[3]), as.Date(t$NDZ_sanemsanas_datums[2]), units = "days")) 
#    #days <- days1 + days2 
#    #rm(days1, days2)
#    
#    #yt <- y2[v, ]
#    #yt$dienas <- days
#  } else {stop("Starpkodi3_91_92: Trūkst izstrādes koda.")}  
#  
#  return(yt)
#}
#
#starpkodi3_91 <- function(y, t, prev, v) {
#  
#  } else if (t$zinkod[2] %in% c("21", "22", "23", "24", "25", "29")) {
#    if(t$zinkod[3] == "92") {
#      if(diff(t$NDZ_sanemsanas_datums[1:2]) != 0 && diff(t$NDZ_sanemsanas_datums[2:3]) == 0) {
#        yt <- y[v, ]
#        yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1 
#      } else {stop("Starpkodi3_91: Trūkst izstrādes koda.")}
#    } else {stop("Starpkodi3_91: Trūkst izstrādes koda.")}
#  } else {stop("Starpkodi3_91: Trūkst izstrādes koda.")}
#  
#  
#  
#  
#  rm(y, t, prev, v)
#  return(yt) 
#}
#
#
#starpkodi3_53 <- function(y2, t, prev, v) {
#  if (t$zinkod[2] == "54" && t$zinkod[3] == "21" && all(diff(t$NDZ_sanemsanas_datums) != 0)) {
#    days1 <- as.numeric(difftime(t$beidz[1], prev, units = "days")) - 1 
#    days2 <- as.numeric(difftime(t$beidz[3], t$sak_darbu[2], units = "days")) 
#    
#    
#    yt <- y2[v, ]
#    yt$dienas <- sum(days1, days2)
#    rm(days1, days2)
#  } else if (t$zinkod[2] == "54" && t$zinkod[3] == "50" && all(!diff(t$NDZ_sanemsanas_datums) == 0)) {
#    days1 <- as.numeric(difftime(t$beidz[1], prev, units = "days")) - 1 
#    days2 <- as.numeric(difftime(t$beidz[3], t$sak_darbu[2], units = "days"))
#    
#    yt <- y2[v, ]
#    yt$dienas <- sum(days1, days2)
#    rm(days1, days2)
#  } else if (t$zinkod[2] == "54" && t$zinkod[3] %in% c("24", "25") && all(!diff(t$NDZ_sanemsanas_datums) == 0)) {
#    days1 <- as.numeric(difftime(t$beidz[1], prev, units = "days")) - 1 
#    days2 <- as.numeric(difftime(t$beidz[3], t$sak_darbu[2], units = "days")) + 1 
#    
#    yt <- y2[v, ]
#    yt$dienas <- sum(days1, days2)
#    rm(days1, days2)
#  } else if (t$zinkod[2] == "25" && t$zinkod[3] == "54" && 
#             diff(t$NDZ_sanemsanas_datums[1:2]) != 0 && diff(t$NDZ_sanemsanas_datums[1:2]) == 0) {
#    yt <- y2[v, ]
#    yt$dienas <- as.numeric(difftime(t$beidz[1], prev, units = "days"))
#  } else if (t$zinkod[2] == "21" && t$zinkod[3] == "54" && 
#             diff(t$NDZ_sanemsanas_datums[1:2]) != 0 && diff(t$NDZ_sanemsanas_datums[2:3]) == 0) {
#    yt <- y2[v, ]
#    yt$dienas <- as.numeric(difftime(t$beidz[1], prev, units = "days")) - 1 
#  } else if (t$zinkod[2] == "54" && t$zinkod[3] == "21" && 
#             diff(t$NDZ_sanemsanas_datums[1:2]) == 0 && diff(t$NDZ_sanemsanas_datums[2:3]) != 0) {
#    yt <- y2[v:(v+1), ]
#    yt <- yt[yt$zinkod == "11", ]
#  } else if (t$zinkod[2] == "22" && t$zinkod[3] == "54") {
#    yt <- y2[v, ]
#    yt$dienas <- as.numeric(difftime(t$beidz[1], prev, units = "days"))
#  } else if (t$zinkod[2] %in% c("25", "40") && t$zinkod[3] == "54" && diff(t$NDZ_sanemsanas_datums[1:2]) != 0 && diff(t$NDZ_sanemsanas_datums[2:3]) == 0) {
#    yt <- y2[v, ]
#    yt$dienas <- as.numeric(difftime(t$beidz[1], prev, units = "days")) - 1
#  } else {
#    stop("Starpkodi3_53: Trūkst izstrādes koda.")
#  }
#  
#  if(is.na(yt$PS_code[1])) {stop("Dienas NA.")}
#  return(yt) 
#}
#

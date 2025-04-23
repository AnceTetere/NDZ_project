starpkodi6_50 <- function(y, t, prev, v) {
  
  yt <- y[v, ]
  
    if (t$zinkod[2] %in% c("41", "51", "54", "92")) {
          yt <- starpkodi6_50_51(y, t, prev, v)
  } else if (t$zinkod[2] %in% c("40", "50", "53", "91")) {
           if (t$zinkod[3] %in% c("41", "51", "54", "92")) {
             if (t$zinkod[4] %in% c("40", "50", "53", "91")) {
               if (t$zinkod[5] %in% c("41", "51", "54", "92")) {
                 if (t$zinkod[6] %in% c("41", "51", "54", "92")) {
                   if (diff(t$NDZ_sanemsanas_datums[1:2]) == 0 && all(diff(t$NDZ_sanemsanas_datums[2:6]) != 0)) {
                     if (t$period[1] == "_____" && t$PS_code[1] == "__________" && t$NM_code[1] == "__________1") {
                       yt$dienas <- as.numeric(sum(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days") - 1, 
                                                   diff(t$NDZ_sanemsanas_datums[3:4]),
                                                   difftime(t$last_date[6], t$NDZ_sanemsanas_datums[6], units = "days") + 1))
                     } else {stop("starpkodi6_91: Trūkst izstrādes koda.")}
                   } else {stop("starpkodi6_91: Trūkst izstrādes koda.")}
                 } else {stop("starpkodi6_91: Trūkst izstrādes koda.")}
               } else {stop("starpkodi6_91: Trūkst izstrādes koda.")}
            } else if (t$zinkod[4] %in% c("41", "51", "54", "92")) {
               if (t$zinkod[5] %in% c("40", "50", "53", "91")) {
                 if (t$zinkod[6] %in% c("41", "51", "54", "92")) {
                   if (all(sapply(c(1,3), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) && 
                       all(sapply(c(2,4,5), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
                     if (t$period[1] == "_____" && t$PS_code[1] == "__________" && t$NM_code[1] == "__________") {
                       yt$dienas <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1, 
                                        as.numeric(diff(t$NDZ_sanemsanas_datums[c(3,5)])),
                                        as.numeric(difftime(t$last_date[6], t$NDZ_sanemsanas_datums[6], units = "days"))) + 1
                     } else {stop("starpkodi6_91: Trūkst izstrādes koda.")}
                   } else {stop("starpkodi6_91: Trūkst izstrādes koda.")}
                 } else {stop("starpkodi6_91: Trūkst izstrādes koda.")}
               } else {stop("starpkodi6_91: Trūkst izstrādes koda.")}
             } else {stop("starpkodi6_91: Trūkst izstrādes koda.")}
           } else {stop("starpkodi6_91: Trūkst izstrādes koda.")}
  } else if (t$zinkod[2] %in% c("21", "22", "23", "24", "25", "29")) {
          if (t$zinkod[3] %in% c("11", "14", "16", "61")) {
            if (t$zinkod[4] %in% c("41", "51", "54", "92")) {
              if (t$zinkod[5] %in% c("11", "14", "16", "61")) {
                if (t$zinkod[6] %in% c("21", "22", "23", "24", "25", "29")) {
                  if (all(diff(t$NDZ_sanemsanas_datums[2:4]) == 0) && 
                      all(sapply(c(1,4,5), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
                    if (t$period[1] == "_____" && t$PS_code[1] == "__________" && t$NM_code[1] == "__________") {
                      t <- t[c(1,5,6),]
                      yt$dienas <- as.numeric(sum(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days") - 1, 
                                            diff(t$NDZ_sanemsanas_datums[2:3])))
              } else {stop("starpkodi6_91: Trūkst izstrādes koda.")}
            } else {stop("starpkodi6_91: Trūkst izstrādes koda.")}
          } else {stop("starpkodi6_91: Trūkst izstrādes koda.")}
        } else {stop("starpkodi6_91: Trūkst izstrādes koda.")}
      } else {stop("starpkodi6_91: Trūkst izstrādes koda.")}
    } else {stop("starpkodi6_91: Trūkst izstrādes koda.")}
  } else {stop("starpkodi6_91: Trūkst izstrādes koda.")}

  
  rm(y, t, prev, v)
  return(yt)
}
   

#} else if (t$zinkod[4] %in% c("21", "22", "23", "24", "25", "29")) {
#  if (t$zinkod[5] %in% c("41", "51", "54", "92")) {
#    if (t$zinkod[6] %in% c("41", "51", "54", "92")) {
#      if (all(sapply(c(1,3:5), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0)) && 
#          diff(t$NDZ_sanemsanas_datums[2:3]) == 0) {
#        yt$dienas <- as.numeric(sum(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days") - 1, 
#                                    diff(t$NDZ_sanemsanas_datums[2:3])))
#      } else {stop("starpkodi6_91: Trūkst izstrādes koda.")}
#    } else {stop("starpkodi6_91: Trūkst izstrādes koda.")}
#  } else {stop("starpkodi6_91: Trūkst izstrādes koda.")}
#  
#      } else if (t$zinkod[4] %in% c("40", "50", "53", "91")) {
#        if (all(sapply(c(2:5), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0)) && diff(t$NDZ_sanemsanas_datums[1:2]) == 0) {
#        } else {stop("starpkodi6_91: Trūkst izstrādes koda.")}
#      } else {stop("starpkodi6_91: Trūkst izstrādes koda.")}
#    } else {stop("starpkodi6_91: Trūkst izstrādes koda.")}    
#    
##GIT: 20240528
#starpkodi6_50 <- function(y2, t, prev, v) {
#  
#  if (all(t$zinkod[c(2, 5)] == "51") && t$zinkod[3] == "50" && t$zinkod[4] == "91" && t$zinkod[6] == "92" && 
#      all(diff(t$NDZ_sanemsanas_datums[4:5]) == 0) && all(diff(t$NDZ_sanemsanas_datums[5:6]) != 0) &&
#      all(diff(t$NDZ_sanemsanas_datums[1:4]) != 0)) {
#    days1 <- as.numeric(difftime(t$beidz_darbu[1], prev, units = "days")) - 1 
#    days2 <- as.numeric(difftime(t$beidz_darbu[3], t$sak_darbu[2], units = "days"))
#    days3 <- as.numeric(difftime(t$last_date[6], t$sak_darbu[6], units = "days")) + 1 
#    
#    yt$dienas <- sum(days1, days2, days3)
#    rm(days1, days2, days3)
#  } else if (all(t$zinkod[c(3,6)] == "50") && all(t$zinkod[c(2,4)] == "51") && t$zinkod[5] == "25" && 
#             all(diff(t$NDZ_sanemsanas_datums[1:5]) != 0) && all(diff(t$NDZ_sanemsanas_datums[5:6]) == 0)) {
#    days1 <- as.numeric(difftime(t$beidz_darbu[1], prev, units = "days")) - 1 
#    days2 <- as.numeric(difftime(t$beidz_darbu[3], t$sak_darbu[2], units = "days"))
#    days3 <- as.numeric(difftime(t$beidz_darbu[5], t$sak_darbu[4], units = "days"))
#    
#    yt$dienas <- sum(days1, days2, days3)
#    rm(days1, days2, days3)
#  } else if (all(t$zinkod[c(1,3)] == "50") && all(t$zinkod[c(2,4,6)] == "51") && t$zinkod[5] == "21" && 
#             all(diff(t$NDZ_sanemsanas_datums[1:5]) != 0) && diff(t$NDZ_sanemsanas_datums[5:6]) == 0) {
#    days1 <- as.numeric(difftime(t$beidz_darbu[1], prev, units = "days")) - 1 
#    days2 <- as.numeric(difftime(t$beidz_darbu[3], t$sak_darbu[2], units = "days"))
#    
#    yt$dienas <- sum(days1, days2)
#    rm(days1, days2)
#  } else {
#    stop("starpkodi6_50: Trūkst izstrādes koda starpkodi.")
#  }
#  
#  if(is.na(yt$PS_code[1])) {stop("Dienas NA.")}
#  return(yt)
#}
# } else if (t$zinkod[1] == "40" && 
#            t$zinkod[2] == "50" && 
#            t$zinkod[3] == "41" && 
#            t$zinkod[4] == "51" && 
#            t$zinkod[5] == "91" && 
#            t$zinkod[6] == "92" && 
#            all(diff(t$NDZ_sanemsanas_datums[1:2]) == 0) && all(diff(t$NDZ_sanemsanas_datums[3:4]) == 0) &&
#            all(diff(t$NDZ_sanemsanas_datums[4:6]) != 0) && all(diff(t$NDZ_sanemsanas_datums[2:3]) != 0)) {
#   yt <- yt[yt$zinkod == "40", ]

#} else if (t$zinkod[1] == "53" && t$zinkod[2] == "53" && 
#           t$zinkod[3] == "54" && t$zinkod[4] == "54" && 
#           t$zinkod[5] == "53" && t$zinkod[6] == "21" && 
#           all(diff(t$NDZ_sanemsanas_datums[2:3]) == 0) && all(diff(t$NDZ_sanemsanas_datums[1:2]) != 0) &&
#           all(diff(t$NDZ_sanemsanas_datums[3:6]) != 0)) {
#  days1 <- as.numeric(difftime(t$beidz_darbu[1], prev, units = "days")) - 1
#  days2 <- as.numeric(difftime(t$beidz_darbu[5], t$sak_darbu[4], units = "days"))
#  
#  yt$dienas <- sum(days1, days2)
#  rm(days1, days2)
#} else if (all(t$zinkod[c(1,3,5)] == "53") && all(t$zinkod[c(2,4)] == "54") && t$zinkod[6] == "21" && all(diff(t$NDZ_sanemsanas_datums) != 0)) {
#  days <- as.numeric(difftime(t$beidz_darbu[1], prev, units = "days")) - 1
#  for(d in seq(2,4,by=2)) {
#    days <- days + as.numeric(difftime(t$beidz_darbu[d+1], t$sak_darbu[d], units = "days"))
#  }
#  
#  yt$dienas <- days
#  rm(days, d)

starpkodi6_91 <- function(y, t, prev, v) {
  
    if (t$zk[2] %in% c("41", "51", "54", "92")) {
      if (t$zk[3] %in% c("40", "50", "53", "91")) {
        if (t$zk[4] %in% c("41", "51", "54", "92")) {
          if (t$zk[5] %in% c("21", "22", "23", "24", "25", "29")) {
            if (t$zk[6] %in% c("41", "51", "54", "92")) {
              if (diff(t$NDZ_sanemsanas_datums[5:6]) == 0 &&
                  all(diff(t$NDZ_sanemsanas_datums[1:5]) != 0)) {
                  yt <- y[v, ]
                  yt$dd <- as.numeric(sum(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days") - 1, 
                                          diff(t$NDZ_sanemsanas_datums[2:3]),
                                          diff(t$NDZ_sanemsanas_datums[4:5]),
                                          difftime(t$last_date[6], t$NDZ_sanemsanas_datums[6], units = "days") + 1))
              } else if (all(sapply(c(1,3,5), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
                         all(sapply(c(2,4), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
                      if (t$period[1] == "202201" && t$PS_code[1] == '_________' && t$NM_code[1] == '_________') {
                        yt <- y[v, ]
                        yt$dd <- sum(sapply(c(1,3,5), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)])))
                      } else {stop("starpkodi6_91: Trūkst izstrādes koda.")}
              } else {stop("starpkodi6_91: Trūkst izstrādes koda.")}
          } else {stop("starpkodi6_91: Trūkst izstrādes koda.")}
        } else {stop("starpkodi6_91: Trūkst izstrādes koda.")}
      } else {stop("starpkodi6_91: Trūkst izstrādes koda.")}
    } else {stop("starpkodi6_91: Trūkst izstrādes koda.")}
  } else if (t$zk[2] %in% c("40", "50", "53", "91")) {
           if (t$zk[3] %in% c("41", "51", "54", "92")) {
             if (t$zk[4] %in% c("40", "50", "53", "91")) {
               if (t$zk[5] %in% c("41", "51", "54", "92")) {
                 if (t$zk[6] %in% c("41", "51", "54", "92")) {
                   if (diff(t$NDZ_sanemsanas_datums[1:2]) == 0 && all(diff(t$NDZ_sanemsanas_datums[2:6]) != 0)) {
                     if (t$period[1] == "202201" && t$PS_code[1] == "_________" && t$NM_code[1] == "_________") {
                       yt <- y[v, ]
                       yt$dd <- as.numeric(sum(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days") - 1, 
                                                   diff(t$NDZ_sanemsanas_datums[3:4]),
                                                   difftime(t$last_date[6], t$NDZ_sanemsanas_datums[6], units = "days") + 1))
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
   

#} else if (t$zk[4] %in% c("21", "22", "23", "24", "25", "29")) {
#  if (t$zk[5] %in% c("41", "51", "54", "92")) {
#    if (t$zk[6] %in% c("41", "51", "54", "92")) {
#      if (all(sapply(c(1,3:5), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0)) && 
#          diff(t$NDZ_sanemsanas_datums[2:3]) == 0) {
#        yt <- y[v, ]
#        yt$dd <- as.numeric(sum(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days") - 1, 
#                                    diff(t$NDZ_sanemsanas_datums[2:3])))
#      } else {stop("starpkodi6_91: Trūkst izstrādes koda.")}
#    } else {stop("starpkodi6_91: Trūkst izstrādes koda.")}
#  } else {stop("starpkodi6_91: Trūkst izstrādes koda.")}
#  
#      } else if (t$zk[4] %in% c("40", "50", "53", "91")) {
#        if (all(sapply(c(2:5), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0)) && diff(t$NDZ_sanemsanas_datums[1:2]) == 0) {
#        } else {stop("starpkodi6_91: Trūkst izstrādes koda.")}
#      } else {stop("starpkodi6_91: Trūkst izstrādes koda.")}
#    } else {stop("starpkodi6_91: Trūkst izstrādes koda.")}    
#  } else if (t$zk[2] %in% c("41", "51", "54", "92")) {
#            if (t$zk[3] %in% c("40", "50", "53", "91")) {
#              if (t$zk[4] %in% c("41", "51", "54", "92")) {
#                if (t$zk[5] %in% c("21", "22", "23", "24", "25", "29")) {
#                  if (t$zk[6] %in% c("41", "51", "54", "92")) {
#                    if (all(sapply(seq(1,6,by=2), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
#                        all(sapply(c(2,4), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
#                      
#                    } else {stop("starpkodi6_91: Trūkst izstrādes koda.")}
#                  } else {stop("starpkodi6_91: Trūkst izstrādes koda.")}
#                } else {stop("starpkodi6_91: Trūkst izstrādes koda.")}
#              } else {stop("starpkodi6_91: Trūkst izstrādes koda.")}
#            } else {stop("starpkodi6_91: Trūkst izstrādes koda.")}
#  } else {stop("starpkodi6_91: Trūkst izstrādes koda.")}
#    
#starpkodi6_50 <- function(y2, t, prev, v) {
#  
#  if (all(t$zk[c(2, 5)] == "51") && t$zk[3] == "50" && t$zk[4] == "91" && t$zk[6] == "92" && 
#      all(diff(t$NDZ_sanemsanas_datums[4:5]) == 0) && all(diff(t$NDZ_sanemsanas_datums[5:6]) != 0) &&
#      all(diff(t$NDZ_sanemsanas_datums[1:4]) != 0)) {
#    days1 <- as.numeric(difftime(t$beidz[1], prev, units = "days")) - 1 
#    days2 <- as.numeric(difftime(t$beidz[3], t$sak[2], units = "days"))
#    days3 <- as.numeric(difftime(t$last_date[6], t$sak[6], units = "days")) + 1 
#    
#    yt <- y2[v, ]
#    yt$dd <- sum(days1, days2, days3)
#    rm(days1, days2, days3)
#  } else if (all(t$zk[c(3,6)] == "50") && all(t$zk[c(2,4)] == "51") && t$zk[5] == "25" && 
#             all(diff(t$NDZ_sanemsanas_datums[1:5]) != 0) && all(diff(t$NDZ_sanemsanas_datums[5:6]) == 0)) {
#    days1 <- as.numeric(difftime(t$beidz[1], prev, units = "days")) - 1 
#    days2 <- as.numeric(difftime(t$beidz[3], t$sak[2], units = "days"))
#    days3 <- as.numeric(difftime(t$beidz[5], t$sak[4], units = "days"))
#    
#    yt <- y2[v, ]
#    yt$dd <- sum(days1, days2, days3)
#    rm(days1, days2, days3)
#  } else if (all(t$zk[c(3,5)] == "50") && all(t$zk[c(2,4)] == "51") && t$zk[6] == "25" && all(diff(t$NDZ_sanemsanas_datums) != 0)) {
#    days1 <- as.numeric(difftime(t$beidz[1], prev, units = "days")) - 1 
#    days2 <- as.numeric(difftime(t$beidz[3], t$sak[2], units = "days"))
#    days3 <- as.numeric(difftime(t$beidz[5], t$sak[4], units = "days"))
#    
#    yt <- y2[v, ]
#    yt$dd <- sum(days1, days2, days3)
#    rm(days1, days2, days3)
#  } else if (all(t$zk[c(1,3)] == "50") && all(t$zk[c(2,4,6)] == "51") && t$zk[5] == "21" && 
#             all(diff(t$NDZ_sanemsanas_datums[1:5]) != 0) && diff(t$NDZ_sanemsanas_datums[5:6]) == 0) {
#    days1 <- as.numeric(difftime(t$beidz[1], prev, units = "days")) - 1 
#    days2 <- as.numeric(difftime(t$beidz[3], t$sak[2], units = "days"))
#    
#    yt <- y2[v, ]
#    yt$dd <- sum(days1, days2)
#    rm(days1, days2)
#  } else {
#    stop("starpkodi6_50: Trūkst izstrādes koda starpkodi.")
#  }
#  
#  if(is.na(yt$PS_code[1])) {stop("DD NA.")}
#  return(yt)
#}
# } else if (t$zk[1] == "40" && 
#            t$zk[2] == "50" && 
#            t$zk[3] == "41" && 
#            t$zk[4] == "51" && 
#            t$zk[5] == "91" && 
#            t$zk[6] == "92" && 
#            all(diff(t$NDZ_sanemsanas_datums[1:2]) == 0) && all(diff(t$NDZ_sanemsanas_datums[3:4]) == 0) &&
#            all(diff(t$NDZ_sanemsanas_datums[4:6]) != 0) && all(diff(t$NDZ_sanemsanas_datums[2:3]) != 0)) {
#   yt <- y[v:(v+1), ]
#   yt <- yt[yt$zk == "40", ]

#} else if (t$zk[1] == "53" && t$zk[2] == "53" && 
#           t$zk[3] == "54" && t$zk[4] == "54" && 
#           t$zk[5] == "53" && t$zk[6] == "21" && 
#           all(diff(t$NDZ_sanemsanas_datums[2:3]) == 0) && all(diff(t$NDZ_sanemsanas_datums[1:2]) != 0) &&
#           all(diff(t$NDZ_sanemsanas_datums[3:6]) != 0)) {
#  days1 <- as.numeric(difftime(t$beidz[1], prev, units = "days")) - 1 
#  days2 <- as.numeric(difftime(t$beidz[5], t$sak[4], units = "days"))
#  
#  yt <- y[v, ]
#  yt$dd <- sum(days1, days2)
#  rm(days1, days2)
#} else if (all(t$zk[c(1,3,5)] == "53") && all(t$zk[c(2,4)] == "54") && t$zk[6] == "21" && all(diff(t$NDZ_sanemsanas_datums) != 0)) {
#  days <- as.numeric(difftime(t$beidz[1], prev, units = "days")) - 1
#  for(d in seq(2,4,by=2)) {
#    days <- days + as.numeric(difftime(t$beidz[d+1], t$sak[d], units = "days"))
#  }
#  
#  yt <- y[v, ]
#  yt$dd <- days
#  rm(days, d)

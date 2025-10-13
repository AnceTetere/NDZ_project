 starpkodi6_51_50 <- function(y, t, prev, v) {  
  
  yt <- y[v, ]
  
  if (t$zinkod[3] %in% c("41", "51", "54", "92")) {
    if (t$zinkod[4] %in% c("40", "50", "53", "91")) {
      if (t$zinkod[5] %in% c("41", "51", "54", "92")) {
        if (t$zinkod[6]  %in% c("21", "22", "23", "24", "25", "29")) {
          if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
            yt$dienas <- sum(sapply(seq(1,6,by=2), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]))) + 1 
          } else if (diff(t$NDZ_sanemsanas_datums[3:4]) == 0 &&
                     all(sapply(c(1,2,4,5), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
            yt$dienas <- sum(sapply(seq(1,6,by=2), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]))) + 1 
            ZERO_minus(t %>% slice(1))
          } else {stop("starpkodi6_51_50: Trūkst izstrādes koda. \n")}
        } else {stop("starpkodi6_51_50: Trūkst izstrādes koda. \n")}
      } else if (t$zinkod[5] %in% c("21", "22", "23", "24", "25", "29")) {
               if (t$zinkod[6] %in% c("41", "51", "54", "92")) {
                  if (all(diff(t$NDZ_sanemsanas_datums[1:5]) != 0) && diff(t$NDZ_sanemsanas_datums[5:6]) == 0) {
                    if ((t$period[1] == '______' && tPS_code ==  '______________' && tNM_code ==  '______________') ||
                        (t$period[1] == '______' && tPS_code ==  '______________' && tNM_code ==  '______________') ||
                        (t$period[1] == '______' && tPS_code ==  '______________' && tNM_code ==  '______________') ||
                        (t$period[1] == '______' && tPS_code ==  '______________' && tNM_code ==  '______________')) {
                         t <- t[c(1:4,6,5),]; rownames(t) <- NULL  
                         yt$dienas <- sum(sapply(seq(1,6,by=2), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)])))
                    } else {stop("starpkodi6_51_50: Trūkst izstrādes koda. \n")}
                  } else if (all(sapply(c(3,5), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) && 
                            all(sapply(c(1,2,4), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
                            if (t$period[1] == '______' && tPS_code ==  '______________' && tNM_code ==  '______________') {
                                t <- t[c(1,2,3,4,6,5),]; rownames(t) <- NULL
                                yt$dienas <- sum(sapply(seq(1,6,by=2), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]))) 
                            } else {stop("starpkodi6_51_50: Trūkst izstrādes koda starpkodi2.")}
                 } else {stop("starpkodi6_51_50: Trūkst izstrādes koda starpkodi2.")}
              } else {stop("starpkodi6_51_50: Trūkst izstrādes koda starpkodi2.")}
      } else if (t$zinkod[5] %in% c("40", "50", "53", "91")) {
        if (t$zinkod[6] %in% c("41", "51", "54", "92")) {
          if (all(diff(t$NDZ_sanemsanas_datums[2:6]) != 0) && diff(t$NDZ_sanemsanas_datums[1:2]) == 0) {
            if (t$period[1] == '______' && t$PS_code[1] ==  '______________' && tNM_code ==  '______________') {
              t <- t[c(1,2,3,5,6),]
              rownames(t) <- NULL
              yt$dienas <- sum(sapply(seq(1,4,by=2), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)])),
                               difftime(t$last_date[5], t$NDZ_sanemsanas_datums[5], units = "days")) + 1
            } else {stop("starpkodi6_51_50: Trūkst izstrādes koda starpkodi2.")}
          } else {stop("starpkodi6_51_50: Trūkst izstrādes koda starpkodi2.")}
        } else {stop("starpkodi6_51_50: Trūkst izstrādes koda starpkodi2.")}
      } else {stop("starpkodi6_51_50: Trūkst izstrādes koda starpkodi2.")}
    } else if (t$zinkod[4] %in% c("41", "51", "54", "92")) {
      if (t$zinkod[5] %in% c("40", "50", "53", "91")) {
        if (t$zinkod[6] %in% c("41", "51", "54", "92")) {
          if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
            yt$dienas <- as.numeric(sum(diff(t$NDZ_sanemsanas_datums[1:2]),
                                        diff(t$NDZ_sanemsanas_datums[4:5]),
                                        difftime(t$last_date[6], t$NDZ_sanemsanas_datums[6], units = "days")))
          } else {stop("starpkodi6_51_50: Trūkst izstrādes koda starpkodi2.")}
        } else {stop("starpkodi6_51_50: Trūkst izstrādes koda starpkodi2.")}
      } else {stop("starpkodi6_51_50: Trūkst izstrādes koda starpkodi2.")}
    } else {stop("starpkodi6_51_50: Trūkst izstrādes koda starpkodi2.")}
  } else if (t$zinkod[3] %in% c("40", "50", "53", "91")) {
    if (t$zinkod[4] %in% c("41", "51", "54", "92")) {
      if (t$zinkod[5] %in% c("40", "50", "53", "91")) {
        if (t$zinkod[6] %in% c("41", "51", "54", "92")) {
          if (all(sapply(c(1,3,5), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
              all(sapply(c(2,4), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
            if (t$period[1] == '______' && t$PS_code[1] ==  '______________' && tNM_code ==  '______________') {
              yt$dienas <- as.numeric(sum(diff(t$NDZ_sanemsanas_datums[1:2]), diff(t$NDZ_sanemsanas_datums[4:3]), diff(t$NDZ_sanemsanas_datums[6:5])))
            } else {stop("starpkodi6_51_50: Trūkst izstrādes koda starpkodi2.")}
          } else {stop("starpkodi6_51_50: Trūkst izstrādes koda starpkodi2.")}
        } else {stop("starpkodi6_51_50: Trūkst izstrādes koda starpkodi2.")}
      } else if (t$zinkod[5] %in% c("21", "22", "23", "24", "25", "29")) {
        if (t$zinkod[6] %in% c("41", "51", "54", "92")) {
          if (all(sapply(c(1,5), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) && all(diff(t$NDZ_sanemsanas_datums[2:5]) != 0)) {
            if (t$period[1] == '______' && t$PS_code[1] ==  '______________' && tNM_code ==  '______________') {
              yt$dienas <- sum(as.numeric(diff(t$NDZ_sanemsanas_datums[c(1,3)])), as.numeric(diff(t$NDZ_sanemsanas_datums[5:6])))
            } else {stop("starpkodi6_51_50: Trūkst izstrādes koda starpkodi2.")}
          } else {stop("starpkodi6_51_50: Trūkst izstrādes koda starpkodi2.")}
        } else {stop("starpkodi6_51_50: Trūkst izstrādes koda starpkodi2.")}
      } else {stop("starpkodi6_51_50: Trūkst izstrādes koda starpkodi2.")}
    } else {stop("starpkodi6_51_50: Trūkst izstrādes koda starpkodi2.")}
  } else {stop("starpkodi6_51_50: Trūkst izstrādes koda starpkodi2.")}
  
  rm(y, t, prev, v)
  return(yt)
}

#  } else if (all(t$zinkod[c(3,6)] == "51") && t$zinkod[4] == "50" && t$zinkod[5] %in% c("21", "25") && 
#             all(diff(t$NDZ_sanemsanas_datums[1:5]) != 0) && all(diff(t$NDZ_sanemsanas_datums[5:6]) == 0)) {
#    days1 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[2], t$NDZ_sanemsanas_datums[1], units = "days"))
#    days2 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[4], t$NDZ_sanemsanas_datums[3], units = "days"))
#    
#    yt <- y[v, ]
#    yt$dienas <- sum(days1, days2)
#    rm(days1, days2)
#  } else if (all(t$zinkod[c(3,5)] == "51") && t$zinkod[6] == "50" && t$zinkod[4] == "21" && 
#             all(diff(t$NDZ_sanemsanas_datums[1:4]) != 0) && all(diff(t$NDZ_sanemsanas_datums[4:6]) == 0)) {
#    days1 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[2], t$NDZ_sanemsanas_datums[1], units = "days"))
#    days2 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[4], t$NDZ_sanemsanas_datums[3], units = "days"))
#    
#    yt <- y[v, ]
#    yt$dienas <- sum(days1, days2)
#    rm(days1, days2)
#  } else if (all(t$zinkod[c(1,4,6)] == "51") && t$zinkod[3] == "50" && t$zinkod[5] == "25" && 
#             all(sapply(seq(1,6,by=2), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
#             all(sapply(c(2,4), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
#    days1 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[2], prev, units = "days")) - 1
#    days2 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[3], t$NDZ_sanemsanas_datums[1], units = "days"))
#    
#    yt <- y[v, ]
#    yt$dienas <- sum(days1, days2)
#    rm(days1, days2)
# } else if (t$zinkod[3] == "51" && 
#            t$zinkod[4] == "40" && 
#            t$zinkod[5] == "41" && 
#            t$zinkod[6] == "50" && 
#            all(diff(t$NDZ_sanemsanas_datums[1:2]) == 0) && all(diff(t$NDZ_sanemsanas_datums[2:5]) != 0) &&
#            all(diff(t$NDZ_sanemsanas_datums[5:6]) == 0)) {
#  yt <- y[v, ]
#  yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[4], t$NDZ_sanemsanas_datums[3], units = "days"))
#} else if (all(t$zinkod[c(3,5)] == "92") && t$zinkod[4] == "91") && t$zinkod[6] == "21" && all(diff(t$NDZ_sanemsanas_datums) != 0)) {
#  days <- 0
#  for(d in seq(1,5,by=2)) {
#    days <- days + as.numeric(difftime(t$NDZ_sanemsanas_datums[d+1], t$NDZ_sanemsanas_datums[d], units = "days"))
#  }
#  
#  yt <- y[v, ]
#  yt$dienas <- days + 1
#  rm(days, d)
#} else if (t$zinkod[3] == "91" &&
#           t$zinkod[4] == "51" && t$zinkod[5] == "40" && t$zinkod[6] == "92" &&
#           all(sapply(seq(1,6,by=2), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
#           all(sapply(seq(2,5,by=2), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
#  yt <- y[v, ]
#  yt$dienas <- 0

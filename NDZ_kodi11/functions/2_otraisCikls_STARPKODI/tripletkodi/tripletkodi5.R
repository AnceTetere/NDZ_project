tripletkodi5 <- function(y3, t, prev, v) {
  
  yt <- y3[v,]
  
  if (t$zinkod[1] %in% c("11", "14", "16", "61")) {
    if (t$zinkod[2] %in% c("40", "50", "53", "91")) {
      if (t$zinkod[3] %in% c("41", "51", "54", "92")) {
        if (t$zinkod[4] %in% c("41", "51", "54", "92")) {
          if (t$zinkod[5] %in% c("40", "50", "53", "91")) {
            if (all(diff(t$NDZ_sanemsanas_datums[2:5]) != 0) && diff(t$NDZ_sanemsanas_datums[1:2]) == 0) {
                 yt$dienas <- sum(sapply(c(1,4), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)])))
               } else {stop("Tripletkodi5 iztrūkst apstrādes koda.")}
             } else {stop("Tripletkodi5 iztrūkst apstrādes koda.")}
           } else {stop("Tripletkodi5 iztrūkst apstrādes koda.")}
        } else {stop("Tripletkodi5 iztrūkst apstrādes koda.")}
    } else {stop("Tripletkodi5 iztrūkst apstrādes koda.")}
  } else if (t$zinkod[1] %in% c("41", "51", "54", "92")) {
    if (t$zinkod[2] %in% c("40", "50", "53", "91")) {
      if (t$zinkod[3] %in% c("21", "22", "23", "24", "25", "29")) {
        if (t$zinkod[4] %in% c("41", "51", "54", "92")) {
          if (t$zinkod[5] %in% c("41", "51", "54", "92")) {
            if (all(sapply(c(1,3), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) && 
                all(sapply(c(2,4), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
              yt$dienas <- sum(sapply(c(1,3), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)])))
            } else {stop("Tripletkodi5 iztrūkst apstrādes koda.")}
          } else {stop("Tripletkodi5 iztrūkst apstrādes koda.")}
        } else {stop("Tripletkodi5 iztrūkst apstrādes koda.")}
      } else {stop("Tripletkodi5 iztrūkst apstrādes koda.")}
    } else {stop("Tripletkodi5 iztrūkst apstrādes koda.")}
  } else if (t$zinkod[1] %in% c("21", "22", "23", "24", "25", "29")) {
    if (t$zinkod[2] %in% c("11", "14", "16", "61")) {
      if (t$zinkod[3] %in% c("40", "50", "53", "91")) {
        if (t$zinkod[4] %in% c("41", "51", "54", "92")) {
          if (t$zinkod[5] %in% c("41", "51", "54", "92")) {
            if (diff(t$NDZ_sanemsanas_datums[4:5]) == 0 && all(diff(t$NDZ_sanemsanas_datums[1:4]) != 0)) {
              if (t$period[1] == "_____" && t$PS_code[1] == "_____" && t$NM_code[1] == "_____") {
                yt$dienas <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")),
                                 as.numeric(diff(t$NDZ_sanemsanas_datums[2:3])),
                                 as.numeric(difftime(t$last_date[5], t$NDZ_sanemsanas_datums[5], units = "days"))) + 1
              } else {stop("Tripletkodi5 iztrūkst apstrādes koda.")}
            } else {stop("Tripletkodi5 iztrūkst apstrādes koda.")}
          } else {stop("Tripletkodi5 iztrūkst apstrādes koda.")}
        } else {stop("Tripletkodi5 iztrūkst apstrādes koda.")}
      } else {stop("Tripletkodi5 iztrūkst apstrādes koda.")}
    } else {stop("Tripletkodi5 iztrūkst apstrādes koda.")}
  } else {stop("Tripletkodi5 iztrūkst apstrādes koda.")}

  rm(y3, t, prev, v)
  if(is.na(yt$PS_code[1])) {stop("Dienas NA.")}
  yt$zinkod <- "combined"  #jo tripletkodu dienu sarēķins
  return(yt)
}

   #  if (t$zinkod[1] == "91" && t$zinkod[2] == "51" && 
#      t$zinkod[3] == "92" && t$zinkod[4] == "50" && t$zinkod[5] == "21" && 
#      t$NDZ_sanemsanas_datums[1] == t$NDZ_sanemsanas_datums[2] &&
#      t$NDZ_sanemsanas_datums[2] != t$NDZ_sanemsanas_datums[3] &&
#      t$NDZ_sanemsanas_datums[3] == t$NDZ_sanemsanas_datums[4] &&
#      t$NDZ_sanemsanas_datums[4] != t$NDZ_sanemsanas_datums[5]) {
#    yt <- y3[v, ]
#    yt$dienas <- 0
#  } else if (t$zinkod[1] == "50" && t$zinkod[2] == "51" && 
#       t$zinkod[3] == "53" && t$zinkod[4] == "54" && t$zinkod[5] == "21" && 
#       t$NDZ_sanemsanas_datums[2] == t$NDZ_sanemsanas_datums[3] &&
#       t$NDZ_sanemsanas_datums[1] != t$NDZ_sanemsanas_datums[2] &&
#       all(diff(t$NDZ_sanemsanas_datums[3:5]) != 0)) {
#    days1 <- as.numeric(difftime(t$beidz_darbu[1], prev, units = "days")) - 1 
#    days2 <- as.numeric(difftime(t$beidz_darbu[5], t$sak_darbu[4], units = "days")) + 1 
#    
#    yt <- y3[v, ]
#    yt$dienas <- sum(days1, days2)
#    rm(days1, days2)
#  } else if (t$zinkod[1] == "91" && t$zinkod[2] == "92" && 
#             t$zinkod[3] == "50" && t$zinkod[4] == "25" && t$zinkod[5] == "51" && 
#             all(sapply(seq(2, 5, by = 2), function(i) all(diff(t$NDZ_sanemsanas_datums[i:(i+1)])== 0))) &&
#             t$NDZ_sanemsanas_datums[1] != t$NDZ_sanemsanas_datums[2]) {
#    yt <- y3[v, ]
#    yt$dienas <- as.numeric(difftime(t$beidz_darbu[1], prev, units = "days")) 
#  } else if (t$zinkod[1] == "92" && t$zinkod[2] == "41" && t$zinkod[3] == "50" && t$zinkod[4] == "25" && t$zinkod[5] == "51" && 
#             all(diff(t$NDZ_sanemsanas_datums[1:3])== 0) && all(diff(t$NDZ_sanemsanas_datums[3:5])!= 0)) {
#    yt <- y3[v, ]
#    yt$dienas <- 0
#  } else {stop("Tripletkodi5 iztrūkst apstrādes koda.")}
#                                    

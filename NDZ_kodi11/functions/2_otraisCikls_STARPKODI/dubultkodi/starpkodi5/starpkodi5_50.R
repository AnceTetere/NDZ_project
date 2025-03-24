starpkodi5_50 <- function(y, t, prev, v) {
  
  yt <- y[v, ]
  
  if (t$zinkod[2] %in% c("41", "51", "54", "92")) {
    yt <- starpkodi5_50_51(y, t, prev, v)
  } else if(t$zinkod[2] == "25" && 
          t$zinkod[3] == "51" && t$zinkod[4] == "11" && 
          t$zinkod[5] == "25" && 
          all(!diff(t$NDZ_sanemsanas_datums[3:5]) == 0) &&
          t$NDZ_sanemsanas_datums[2] == t$NDZ_sanemsanas_datums[3] &&
          t$NDZ_sanemsanas_datums[1] != t$NDZ_sanemsanas_datums[2]) {
  days1 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) #atvaļinājums, bet atgriežoties tiek atlaists tur summējas viena diena, tāpēc šeit to neatņemu
  days2 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[5], t$NDZ_sanemsanas_datums[4], units = "days")) + 1 #jo atlaišana
  
  yt$dienas <- sum(days1, days2)
  rm(days1, days2)
} else if(all(t$zinkod[2:3] %in% c("40", "50", "53", "91")) && t$zinkod[5] == "51" && t$zinkod[4] == "25" && 
          all(diff(t$NDZ_sanemsanas_datums[1:4]) != 0) && diff(t$NDZ_sanemsanas_datums[4:5]) == 0) {
  yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[3], prev, units = "days"))
} else if(t$zinkod[2] == "21" && all(t$zinkod[3:5] == "51")) {
  yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1 
} else if (t$zinkod[2] %in% c("40", "50", "53", "91")) {
         if (t$zinkod[3] %in% c("41", "51", "54", "92")) {
           if (t$zinkod[4] %in% c("41", "51", "54", "92")) {
              if (t$zinkod[5] %in% c("40", "50", "53", "91")) {
                if (diff(t$NDZ_sanemsanas_datums[2:3]) == 0 && all(sapply(c(1,3,4), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
                  yt$dienas <- sum(  as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1, 
                                     as.numeric(diff(t$NDZ_sanemsanas_datums[2:3])),
                                   as.numeric(difftime(t$NDZ_sanemsanas_datums[5], t$NDZ_sanemsanas_datums[4], units = "days"))) 
                  } else {stop("Starpkodi5_50: iztrūkst apstrādes koda.")}
              } else {stop("Starpkodi5_50: iztrūkst apstrādes koda.")}
           } else if (t$zinkod[4] %in% c("40", "50", "53", "91")) {
             if (t$zinkod[5] %in% c("41", "51", "54", "92")) {
               if (all(sapply(c(1,3), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0)) ||
                   all(sapply(c(2,4), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0))) {
                 yt$dienas <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1, 
                                  sapply(c(2,4), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]))) 
               } else {stop("Starpkodi5_50: iztrūkst apstrādes koda.")}
             } else {stop("Starpkodi5_50: iztrūkst apstrādes koda.")}
           } else if (t$zinkod[4] %in% c("21", "22", "23", "24", "25", "29")) {
             if (t$zinkod[5] %in% c("41", "51", "54", "92")) {
               if (diff(t$NDZ_sanemsanas_datums[4:5]) == 0 && all(diff(t$NDZ_sanemsanas_datums[1:4]) != 0)) {
                 if (t$period[1] == "_____" && t$PS_code[1] == "______" && t$NM_code[1] == "_____") {
                   yt$dienas <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[2], prev, units = "days")) - 1, 
                                    as.numeric(diff(t$NDZ_sanemsanas_datums[4:5])))
                 } else {stop("Starpkodi5_50: iztrūkst apstrādes koda.")}
               } else {stop("Starpkodi5_50: iztrūkst apstrādes koda.")}
             } else {stop("Starpkodi5_50: iztrūkst apstrādes koda.")}
           } else {stop("Starpkodi5_50: iztrūkst apstrādes koda.")}
      } else if (t$zinkod[3] %in% c("21", "22", "23", "24", "25", "29")) {
        if (t$zinkod[4] %in% c("41", "51", "54", "92")) {
          if (t$zinkod[5] %in% c("41", "51", "54", "92")) {
            if (all(sapply(c(1,3), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) && all(sapply(c(2,4), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
              if(t$period[1] == "______" && t$PS_code[1] == "_____" && t$NM_code[1] == "_____") {
                yt$dienas <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[2], prev, units = "days")) - 1, 
                                 as.numeric(diff(t$NDZ_sanemsanas_datums[3:4])))
              } else {stop("Starpkodi5_50: iztrūkst apstrādes koda.")}
            } else {stop("Starpkodi5_50: iztrūkst apstrādes koda.")}
          } else {stop("Starpkodi5_50: iztrūkst apstrādes koda.")}
        } else {stop("Starpkodi5_50: iztrūkst apstrādes koda.")}
      } else if (t$zinkod[3] %in% c("41", "51", "54", "92")) {
        if (t$zinkod[4] %in% c("41", "51", "54", "92")) {
          if (t$zinkod[5] %in% c("21", "22", "23", "24", "25", "29")) {
            if (all(sapply(c(1,3,4), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0)) && diff(t$NDZ_sanemsanas_datums[2:3]) == 0) {
              if(t$period[1] == "______" && t$PS_code[1] == "_____" && t$NM_code[1] == "_____") {
                t <- t[c(1,3,2,4,5), ]
                yt$dienas <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[2], prev, units = "days")) - 1, 
                                 sapply(c(2,4), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]))) + 1
              } else {stop("Starpkodi5_50: iztrūkst apstrādes koda.")}
            } else {stop("Starpkodi5_50: iztrūkst apstrādes koda.")}
          } else {stop("Starpkodi5_50: iztrūkst apstrādes koda.")}
        } else {stop("Starpkodi5_50: iztrūkst apstrādes koda.")}
      } else {stop("Starpkodi5_50: iztrūkst apstrādes koda.")}
      } else {stop("Starpkodi5_50: iztrūkst apstrādes koda.")}


  if(is.na(yt$PS_code[1])) {stop("Dienas NA.")}
return(yt)
}
#starpkodi5_91 <- function(y2, t, prev, v) {
#  
#  if (t$zinkod[2] == "92" && t$NDZ_sanemsanas_datums[1] != t$NDZ_sanemsanas_datums[2] && 
#      t$zinkod[3] == "91" && t$zinkod[4] == "92" && t$NDZ_sanemsanas_datums[3] != t$NDZ_sanemsanas_datums[4] && 
#      t$zinkod[5] == "40" && t$NDZ_sanemsanas_datums[4] <= t$NDZ_sanemsanas_datums[5]) {
#    yt <- y2[v:(v+1), ]
#    
#    days1 <- as.numeric(difftime(t$last_date[5], t$beidz_darbu[5], units = "days")) + 1 #jo rēķinu atvaļinājumu apgriezti, priekš atņemšanas
#    days2 <- yt$dienas[yt$zinkod == "91"]
#    days <- days2 - days1
#    rm(days1, days2)
#    
#    yt <- yt[1, ]
#    yt$dienas <- days
#  } else if (t$zinkod[2] == "92" && t$zinkod[3] == "91" && t$zinkod[4] == "92" && t$zinkod[5] == "25") {
#    days1 <- as.numeric(difftime(t$beidz_darbu[1], prev, units = "days")) - 1  apgriezti, Norādītajā datumā persona jau ir brīva.
#    days2 <- as.numeric(difftime(t$beidz_darbu[3], t$sak_darbu[2], units = "days"))
#    days3 <- as.numeric(difftime(t$beidz_darbu[5], t$sak_darbu[4], units = "days"))
#    
#    yt <- y2[v, ]
#    yt$dienas <- sum(days1, days2, days3)
#    rm(days1, days2, days3)
#  }  else if(t$zinkod[2] == "92" && t$zinkod[3] == "91" && t$zinkod[4] == "92" && 
#             t$zinkod[5] == "21" && all(diff(t$NDZ_sanemsanas_datums) != 0)) {
#    days1 <- as.numeric(difftime(t$beidz_darbu[1], prev, units = "days")) - 1 
#    days2 <- as.numeric(difftime(t$beidz_darbu[3], t$sak_darbu[2], units = "days"))
#    days3 <- as.numeric(difftime(t$beidz_darbu[5], t$sak_darbu[4], units = "days"))
#    
#    yt <- y2[v, ]
#    yt$dienas <- sum(days1, days2, days3)
#    rm(days1, days2, days3)
#  } else if(t$zinkod[2] == "91" && t$zinkod[3] == "92" && t$zinkod[4] == "92" && 
#            t$zinkod[5] == "25" && all(diff(t$NDZ_sanemsanas_datums[2:3]) == 0) && all(diff(t$NDZ_sanemsanas_datums[1:2]) != 0) && all(diff(t$NDZ_sanemsanas_datums[3:4]) != 0)) {
#    days1 <- as.numeric(difftime(t$beidz_darbu[1], prev, units = "days")) - 1 
#    days2 <- as.numeric(difftime(t$beidz_darbu[5], t$sak_darbu[4], units = "days")) + 1 
#    
#    yt <- y2[v, ]
#    yt$dienas <- sum(days1, days2)
#    rm(days1, days2)
#  } else if(t$zinkod[3] == "91" && all(t$zinkod[c(2,4)] == "92") && t$zinkod[5] == "50" && all(diff(t$NDZ_sanemsanas_datums) != 0)) {
#    days1 <- as.numeric(difftime(t$beidz_darbu[1], prev, units = "days")) - 1 
#    days2 <- as.numeric(difftime(t$beidz_darbu[3], t$sak_darbu[2], units = "days"))
#    days3 <- as.numeric(difftime(t$beidz_darbu[5], t$sak_darbu[4], units = "days"))
#    
#    yt <- y2[v, ]
#    yt$dienas <- sum(days1, days2, days3)
#    rm(days1, days2, days3)
#  } else if(t$zinkod[2] == "51" && all(t$zinkod[c(3,5)] == "92") && t$zinkod[4] == "91" && 
#            all(diff(t$NDZ_sanemsanas_datums[2:5]) != 0) && diff(t$NDZ_sanemsanas_datums[1:2]) == 0) {
#    days1 <- as.numeric(difftime(t$beidz_darbu[4], t$sak_darbu[3], units = "days"))
#    days2 <- as.numeric(difftime(t$last_date[5], t$sak_darbu[5], units = "days")) + 1 
#    
#    yt <- y2[v, ]
#    yt$dienas <- sum(days1, days2)
#    rm(days1, days2)
#  } else {
#    stop("Starpkodi5_91: Trūkst izstrādes koda.")
#  }
#  
#  if(is.na(yt$PS_code[1])) {stop("Dienas NA.")}
#  return(yt) 
#}#

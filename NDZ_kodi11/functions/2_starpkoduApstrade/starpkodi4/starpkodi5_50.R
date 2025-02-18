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
  days1 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) 
  days2 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[5], t$NDZ_sanemsanas_datums[4], units = "days")) + 1 
  
  yt$dd <- sum(days1, days2)
  rm(days1, days2)
} else if(all(t$zinkod[2:3] %in% c("40", "50", "53", "91")) && t$zinkod[5] == "51" && t$zinkod[4] == "25" && 
          all(diff(t$NDZ_sanemsanas_datums[1:4]) != 0) && diff(t$NDZ_sanemsanas_datums[4:5]) == 0) {
  yt$dd <- as.numeric(difftime(t$NDZ_sanemsanas_datums[3], prev, units = "days"))
} else if(t$zinkod[2] == "21" && all(t$zinkod[3:5] == "51")) {
  yt$dd <- as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1 
} else if (t$zinkod[2] %in% c("40", "50", "53", "91")) {
         if (t$zinkod[3] %in% c("41", "51", "54", "92")) {
           if (t$zinkod[4] %in% c("41", "51", "54", "92")) {
              if (t$zinkod[5] %in% c("40", "50", "53", "91")) {
                if (diff(t$NDZ_sanemsanas_datums[2:3]) == 0 && all(sapply(c(1,3,4), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
                  yt$dd <- sum(  as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1, 
                                     as.numeric(diff(t$NDZ_sanemsanas_datums[2:3])),
                                   as.numeric(difftime(t$NDZ_sanemsanas_datums[5], t$NDZ_sanemsanas_datums[4], units = "days"))) 
                  } else {stop("Starpkodi5_50: iztrūkst apstrādes koda.")}
              } else {stop("Starpkodi5_50: iztrūkst apstrādes koda.")}
           } else if (t$zinkod[4] %in% c("40", "50", "53", "91")) {
             if (t$zinkod[5] %in% c("41", "51", "54", "92")) {
               if (all(sapply(c(1,3), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0)) ||
                   all(sapply(c(2,4), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0))) {
                 yt$dd <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1, 
                                  sapply(c(2,4), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]))) 
               } else {stop("Starpkodi5_50: iztrūkst apstrādes koda.")}
             } else {stop("Starpkodi5_50: iztrūkst apstrādes koda.")}
           } else if (t$zinkod[4] %in% c("21", "22", "23", "24", "25", "29")) {
             if (t$zinkod[5] %in% c("41", "51", "54", "92")) {
               if (diff(t$NDZ_sanemsanas_datums[4:5]) == 0 && all(diff(t$NDZ_sanemsanas_datums[1:4]) != 0)) {
                 if (t$period[1] == "_______" && t$PS_code[1] == "_______" && t$NM_code[1] == "_______") {
                   yt$dd <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[2], prev, units = "days")) - 1, 
                                    as.numeric(diff(t$NDZ_sanemsanas_datums[4:5])))
                 } else {stop("Starpkodi5_50: iztrūkst apstrādes koda.")}
               } else {stop("Starpkodi5_50: iztrūkst apstrādes koda.")}
             } else {stop("Starpkodi5_50: iztrūkst apstrādes koda.")}
           } else {stop("Starpkodi5_50: iztrūkst apstrādes koda.")}
      } else if (t$zinkod[3] %in% c("21", "22", "23", "24", "25", "29")) {
        if (t$zinkod[4] %in% c("41", "51", "54", "92")) {
          if (t$zinkod[5] %in% c("41", "51", "54", "92")) {
            if (all(sapply(c(1,3), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) && all(sapply(c(2,4), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
              if(t$period[1] == "______" && t$PS_code[1] == "_______" && t$NM_code[1] == "_______") {
                yt$dd <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[2], prev, units = "days")) - 1, 
                                 as.numeric(diff(t$NDZ_sanemsanas_datums[3:4])))
              } else {stop("Starpkodi5_50: iztrūkst apstrādes koda.")}
            } else {stop("Starpkodi5_50: iztrūkst apstrādes koda.")}
          } else {stop("Starpkodi5_50: iztrūkst apstrādes koda.")}
        } else {stop("Starpkodi5_50: iztrūkst apstrādes koda.")}
      } else {stop("Starpkodi5_50: iztrūkst apstrādes koda.")}
      } else {stop("Starpkodi5_50: iztrūkst apstrādes koda.")}


  if(is.na(yt$PS_code[1])) {stop("Dienas NA.")}
return(yt)
}

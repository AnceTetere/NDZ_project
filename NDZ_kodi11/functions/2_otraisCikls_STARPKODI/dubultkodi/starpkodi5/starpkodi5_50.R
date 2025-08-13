starpkodi5_50 <- function(y, t, prev, v) {
  
  yt <- y[v, ]
  
  if (t$zinkod[2] %in% c("41", "51", "54", "92")) {
        yt <- starpkodi5_50_51(y, t, prev, v)
  } else if (t$zinkod[2] %in% c("21", "22", "23", "24", "25", "29")) {
        yt <- starpkodi5_50_25(y, t, prev, v)
} else if (t$zinkod[2] %in% c("40", "50", "53", "91")) {
         if (t$zinkod[3] %in% c("41", "51", "54", "92")) {
           if (t$zinkod[4] %in% c("41", "51", "54", "92")) {
             if (t$zinkod[5] %in% c("21", "22", "23", "24", "25", "29")) {
               if (diff(t$NDZ_sanemsanas_datums[2:3]) == 0 && all(sapply(c(1,3,4), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
                 if ((t$period[1] == '______' && t$PS_code[1] ==  '______________' && t$NM_code[1] ==  '______________') ||
                     (t$period[1] == '______' && t$PS_code[1] ==  '______________' && t$NM_code[1] ==  '______________')) {
                   t <- t[c(1,3,2,4,5), ]
                   yt$dienas <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[2], prev, units = "days")) - 1, 
                                    sapply(c(2,4), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]))) + 1
                 } else {stop("Starpkodi5_50: iztrūkst apstrādes koda.")} 
               } else if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
                 if ((t$period[1] == '______' && t$PS_code[1] ==  '______________' && t$NM_code[1] ==  '______________') ||
                     (t$period[1] == '______' && t$PS_code[1] ==  '______________' && t$NM_code[1] ==  '______________')) {
                         yt$dienas <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[2], prev, units = "days")) - 1, 
                                          as.numeric(diff(t$NDZ_sanemsanas_datums[4:5]))) + 1
                } else {stop("Starpkodi5_50: iztrūkst apstrādes koda.")} 
               } else {stop("Starpkodi5_50: iztrūkst apstrādes koda.")}
             } else if (t$zinkod[5] %in% c("40", "50", "53", "91")) {
               if (diff(t$NDZ_sanemsanas_datums[2:3]) == 0 && all(sapply(c(1,3,4), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
                 if ((t$period[1] == '______' && t$PS_code[1] ==  '______________' && t$NM_code[1] ==  '______________') ||
                     (t$period[1] == '______' && t$PS_code[1] ==  '______________' && t$NM_code[1] ==  '______________') ||
                     (t$period[1] == '______' && t$PS_code[1] ==  '______________' && t$NM_code[1] ==  '______________')) {
                   t <- t[c(1,3,2,4,5), ]
                   yt$dienas <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1, 
                                    sapply(c(2,4), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]))) + 1
                 } else {stop("Starpkodi5_50: iztrūkst apstrādes koda.")} 
               } else if (all(sapply(c(2,4), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) && 
                          all(sapply(c(1,3), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {                 
                 if (t$period[1] == '______' && t$PS_code[1] ==  '______________' && t$NM_code[1] ==  '___
                   t <- t[c(1,3,2,4,5), ]
                   yt$dienas <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1, 
                                    sapply(c(2,4), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]))) + 1
                 } else {stop("Starpkodi5_50: iztrūkst apstrādes koda.")} 
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
                 if (t$period[1] == '______' && t$PS_code[1] ==  '______________' && t$NM_code[1] ==  '______________') {
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
              if(t$period[1] == '______' && t$PS_code[1] ==  '______________' && t$NM_code[1] ==  '______________') {
                yt$dienas <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[2], prev, units = "days")) - 1, 
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

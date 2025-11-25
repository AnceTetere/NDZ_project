starpkodi10 <- function(y, t, prev, v) {
  
  yt <- y[v, ]
  
  if (t$zinkod[1] %in% c("11", "14", "16", "61")) { 
   if (t$zinkod[2] %in% c("40", "50", "53", "91")) { 
    if (t$zinkod[3] %in% c("41", "51", "54", "92")) {
     if (t$zinkod[4] %in% c("41", "51", "54", "92")) {
      if (t$zinkod[5] %in% c("40", "50", "53", "91")) {
       if (t$zinkod[6] %in% c("41", "51", "54", "92")) {
        if (t$zinkod[7] %in% c("41", "51", "54", "92")) {
         if (t$zinkod[8] %in% c("40", "50", "53", "91")) {
          if (t$zinkod[9] %in% c("41", "51", "54", "92")) {
           if (t$zinkod[10] %in% c("41", "51", "54", "92")) {
            if (all(diff(t$NDZ_sanemsanas_datums[c(1:7,9:10)]) != 0) && diff(t$NDZ_sanemsanas_datums[8:9]) == 0) {
               yt$dienas <- sum(sapply(c(1,4,7), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)])),
                                difftime(t$last_date[10], t$NDZ_sanemsanas_datums[10], units = "days") + 1)
             } else {stop("Starpkodi10 iztrūkst apstrādes koda.")}
             } else {stop("Starpkodi10 iztrūkst apstrādes koda.")}
             } else {stop("Starpkodi10 iztrūkst apstrādes koda.")}
             } else {stop("Starpkodi10 iztrūkst apstrādes koda.")}
             } else {stop("Starpkodi10 iztrūkst apstrādes koda.")}
             } else {stop("Starpkodi10 iztrūkst apstrādes koda.")}
             } else {stop("Starpkodi10 iztrūkst apstrādes koda.")}
    } else if (t$zinkod[4] %in% c("40", "50", "53", "91")) {
             if (t$zinkod[5] %in% c("41", "51", "54", "92")) {
               if (t$zinkod[6] %in% c("40", "50", "53", "91")) {
                  if (t$zinkod[7] %in% c("41", "51", "54", "92")) {
                    if (t$zinkod[8] %in% c("40", "50", "53", "91")) {
                       if (t$zinkod[9] %in% c("41", "51", "54", "92")) {
                          if (t$zinkod[10] %in% c("40", "50", "53", "91")) {
                            if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
                               yt$dienas <- sum(sapply(seq(1,10,by=2), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)])))
                           } else {stop("Starpkodi10 iztrūkst apstrādes koda.")}
                         } else {stop("Starpkodi10 iztrūkst apstrādes koda.")}
                       } else {stop("Starpkodi10 iztrūkst apstrādes koda.")}
                     } else {stop("Starpkodi10 iztrūkst apstrādes koda.")}
                   } else {stop("Starpkodi10 iztrūkst apstrādes koda.")}
                 } else {stop("Starpkodi10 iztrūkst apstrādes koda.")}
             } else if (t$zinkod[5] %in% c("40", "50", "53", "91")) {
               if (t$zinkod[6] %in% c("41", "51", "54", "92")) {
                 if (t$zinkod[7] %in% c("41", "51", "54", "92")) {
                   if (t$zinkod[8] %in% c("40", "50", "53", "91")) {
                     if (t$zinkod[9] %in% c("41", "51", "54", "92")) {
                       if (t$zinkod[10] %in% c("41", "51", "54", "92")) {
                         if (diff(t$NDZ_sanemsanas_datums[5:6]) == 0 && all(sapply(c(1:4,6:9), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
                           if (t$period[1] == '______' && t$PS_code[1] ==  '______________' && t$NM_code[1] ==  '______________') {
                                yt$dienas <- sum(sapply(c(seq(1,7,by=2)), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)])),
                                                 as.numeric(difftime(t$last_date[10], t$NDZ_sanemsanas_datums[10], units = 'days'))) + 1
                           } else {stop("Starpkodi10 iztrūkst apstrādes koda.")}
                         } else {stop("Starpkodi10 iztrūkst apstrādes koda.")}
                       } else {stop("Starpkodi10 iztrūkst apstrādes koda.")}
                     } else {stop("Starpkodi10 iztrūkst apstrādes koda.")}
                   } else {stop("Starpkodi10 iztrūkst apstrādes koda.")}
                 } else {stop("Starpkodi10 iztrūkst apstrādes koda.")}
               } else {stop("Starpkodi10 iztrūkst apstrādes koda.")}
             }else {stop("Starpkodi10 iztrūkst apstrādes koda.")}
    } else {stop("Starpkodi10 iztrūkst apstrādes koda.")}
   } else {stop("Starpkodi10 iztrūkst apstrādes koda.")}
   } else {stop("Starpkodi10 iztrūkst apstrādes koda.")}
} else if (t$zinkod[1] %in% c("41", "51", "54", "92")) {
    if (t$zinkod[2] %in% c("40", "50", "53", "91")) { 
      if (t$zinkod[3] %in% c("41", "51", "54", "92")) {
        if (t$zinkod[4] %in% c("40", "50", "53", "91")) {
          if (t$zinkod[5] %in% c("41", "51", "54", "92")) {
            if (t$zinkod[6] %in% c("40", "50", "53", "91")) {
              if (t$zinkod[7] %in% c("41", "51", "54", "92")) {
                if (t$zinkod[8] %in% c("40", "50", "53", "91")) {
                  if (t$zinkod[9] %in% c("41", "51", "54", "92")) {
                    if (t$zinkod[10] %in% c("21", "22", "23", "24", "25", "29")) {
              yt$dienas <- sum(sapply(seq(1,10,by=2), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]))) + 1
      } else {stop("Starpkodi10 iztrūkst apstrādes koda.")}
    } else {stop("Starpkodi10 iztrūkst apstrādes koda.")}
  } else {stop("Starpkodi10 iztrūkst apstrādes koda.")}
  } else {stop("Starpkodi10 iztrūkst apstrādes koda.")}
  } else {stop("Starpkodi10 iztrūkst apstrādes koda.")}
  } else {stop("Starpkodi10 iztrūkst apstrādes koda.")}
  } else {stop("Starpkodi10 iztrūkst apstrādes koda.")}
  } else {stop("Starpkodi10 iztrūkst apstrādes koda.")}
  } else {stop("Starpkodi10 iztrūkst apstrādes koda.")}
} else if (t$zinkod[1] %in% c("40", "50", "53", "91")) {
         if (t$zinkod[2] %in% c("41", "51", "54", "92")) {
           if (t$zinkod[3] %in% c("40", "50", "53", "91")) {
             if (t$zinkod[4] %in% c("41", "51", "54", "92")) {
               if (t$zinkod[5] %in% c("21", "22", "23", "24", "25", "29")) {
                 if (t$zinkod[6] %in% c("40", "50", "53", "91")) {
                   if (t$zinkod[7] %in% c("41", "51", "54", "92")) {
                     if (t$zinkod[8] %in% c("40", "50", "53", "91")) {
                       if (t$zinkod[9] %in% c("41", "51", "54", "92")) {
                         if (t$zinkod[10] %in% c("40", "50", "53", "91")) {
                           if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
                             if (t$period[1] == '______' && t$PS_code[1] ==  '______________' && t$NM_code[1] ==  '______________') {
                               yt$dienas <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1,
                                                sapply(c(2,4), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]))) + 1
              
                             } else {stop("Starpkodi10 iztrūkst apstrādes koda.")}
                          } else {stop("Starpkodi10 iztrūkst apstrādes koda.")}
                      } else {stop("Starpkodi10 iztrūkst apstrādes koda.")}
                  } else {stop("Starpkodi10 iztrūkst apstrādes koda.")}
              } else {stop("Starpkodi10 iztrūkst apstrādes koda.")}
            } else {stop("Starpkodi10 iztrūkst apstrādes koda.")}
          } else {stop("Starpkodi10 iztrūkst apstrādes koda.")}
        } else {stop("Starpkodi10 iztrūkst apstrādes koda.")}
      } else {stop("Starpkodi10 iztrūkst apstrādes koda.")}
    } else {stop("Starpkodi10 iztrūkst apstrādes koda.")}
  } else {stop("Starpkodi10 iztrūkst apstrādes koda.")}
} else {stop("Starpkodi10 iztrūkst apstrādes koda.")}

  if(is.na(yt$PS_code[1])) {stop("Dienas NA.")}
  yt$zinkod <- "combined"  #jo starpkodu dienu sarēķins
  return(yt)
}

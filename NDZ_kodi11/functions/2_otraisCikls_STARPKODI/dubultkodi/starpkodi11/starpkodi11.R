starpkodi11 <- function(y, t, prev, v) {

  yt <- y[v, ]
  
  if (t$zinkod[1] %in% c("11", "14", "16", "61")) {
    if (t$zinkod[2] %in% c("40", "50", "53", "91")) {
      if (t$zinkod[3] %in% c("41", "51", "54", "92")) {
        if (t$zinkod[4] %in% c("40", "50", "53", "91")) {
            if (all(t$zinkod[seq(6,11,by=2)] %in% c("40", "50", "53", "91")) && 
                all(t$zinkod[seq(5,11,by=2)] %in% c("41", "51", "54", "92")) && 
                all(diff(t$NDZ_sanemsanas_datums) != 0)) {
                      yt$dienas <- sum(sapply(seq(1, 10, by=2), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)])),
                                       difftime(t$last_date[11], t$NDZ_sanemsanas_datums[11], units = "days")) + 1
            } else {stop("Starpkodi11 iztrūkst apstrādes koda.")}
        } else if (t$zinkod[4] %in% c("41", "51", "54", "92")) {
            if (all(t$zinkod[seq(6,11,by=2)] %in% c("41", "51", "54", "92")) && 
                all(t$zinkod[seq(5,11,by=2)] %in% c("40", "50", "53", "91")) && 
                all(diff(t$NDZ_sanemsanas_datums) != 0)) {
                
                if (t$period[1] == '______' && t$PS_code[1] ==  '______________' &&  t$NM_code[1] ==  '______________') {
                    yt$dienas <- sum(sapply(c(1,4,6,8,10), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)])))
                } else {stop("Starpkodi11 iztrūkst apstrādes koda.")}
          } else {stop("Starpkodi11 iztrūkst apstrādes koda.")}
        } else {stop("Starpkodi11 iztrūkst apstrādes koda.")}
      } else {stop("Starpkodi11 iztrūkst apstrādes koda.")}
    } else {stop("Starpkodi11 iztrūkst apstrādes koda.")}
  } else if (t$zinkod[1] %in% c("40", "50", "53", "91")) {
             if (all(t$zinkod[c(2,6,7)] %in% c("40", "50", "53", "91")) && 
                     all(t$zinkod[c(3:5,8:10)] %in% c("41", "51", "54", "92")) && 
                     t$zinkod[11] %in% c("40", "50", "53", "91") && 
                     all(sapply(c(1,3,6,8), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
                     all(sapply(c(2,4,5,7,9,10), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
            yt$dienas <- as.numeric(sum(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days") - 1,
                                        diff(t$NDZ_sanemsanas_datums[5:6]), 
                                        diff(t$NDZ_sanemsanas_datums[10:11])))
             } else if (all(t$zinkod[c(3,5,7,9)] %in% c("40", "50", "53", "91")) && 
                        all(t$zinkod[c(2,4,6,8,11)] %in% c("41", "51", "54", "92")) &&
                        t$zinkod[10] %in% c("21", "22", "23", "24", "25", "29") &&
                        all(diff(t$NDZ_sanemsanas_datums[1:10]) != 0) && diff(t$NDZ_sanemsanas_datums[10:11]) == 0) {               
               if ((t$period[1] == '______' && t$PS_code[1] ==  '______________' && t$NM_code[1] ==  '______________') ||
                   (t$period[1] == '______' && t$PS_code[1] ==  '______________' && t$NM_code[1] ==  '______________')) {
                 yt$dienas <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1,
                                  sapply(seq(2,10,by=2), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]))) 
               } else {stop("Starpkodi11 iztrūkst apstrādes koda.")}
             } else {stop("Starpkodi11 iztrūkst apstrādes koda.")}
  } else if (t$zinkod[1] %in% c("41", "51", "54", "92")) { 
             if (all(t$zinkod[seq(2,11,by=2)] %in% c("40", "50", "53", "91")) && 
                      all(t$zinkod[seq(3,11,by=2)] %in% c("41", "51", "54", "92")) && 
                      all(diff(t$NDZ_sanemsanas_datums) != 0)) {
                     yt$dienas <- sum(sapply(seq(1,10,by=2), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)])),
                                      difftime(t$last_date[11], t$NDZ_sanemsanas_datums[11], units = "days") + 1)
           } else if (all(t$zinkod[c(3,5,7,10)] %in% c("40", "50", "53", "91")) && 
                      all(t$zinkod[c(2,4,6,8,11)] %in% c("41", "51", "54", "92")) &&
                      t$zinkod[9] %in% c("21", "22", "23", "24", "25", "29") &&
                      all(diff(t$NDZ_sanemsanas_datums) != 0)) {                       
                         if (t$period[1] == '______' && t$PS_code[1] ==  '______________' && t$NM_code[1] ==  '______________') {
                             yt$dienas <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1,
                                              sapply(seq(2,9,by=2), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]))) + 1 
                         } else {stop("Starpkodi11 iztrūkst apstrādes koda.")}
           } else {stop("Starpkodi11 iztrūkst apstrādes koda.")}
  } else {stop("Starpkodi11 iztrūkst apstrādes koda.")}
  
  if(is.na(yt$PS_code[1])) {stop("Dienas NA.")}
  yt$zinkod <- "combined"  #jo starpkodu dienu sarēķins
  
  rm(y, t, prev, v)
  return(yt)
}

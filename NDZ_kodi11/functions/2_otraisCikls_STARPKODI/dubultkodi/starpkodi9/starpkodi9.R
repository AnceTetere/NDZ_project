starpkodi9 <- function(y, t, prev, v) {
  
  yt <- y[v, ]
  
  if (t$zinkod[1] %in% c("40", "50", "53", "91")) {
    yt <- starpkodi9_50(y, t, prev, v) 
  } else if (t$zinkod[1] %in% c("11", "14", "16", "61") && 
             t$zinkod[2] %in% c("40", "50", "53", "91") && 
             t$zinkod[3] %in% c("41", "51", "54", "92") && 
             t$zinkod[4] %in% c("40", "50", "53", "91") && 
             t$zinkod[5] %in% c("41", "51", "54", "92") && 
             t$zinkod[6] %in% c("40", "50", "53", "91") && 
             t$zinkod[7] %in% c("41", "51", "54", "92") && 
             t$zinkod[8] %in% c("40", "50", "53", "91")) { 
             if (t$zinkod[9] %in% c("41", "51", "54", "92")) {
               if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
                 yt$dienas <- sum(sapply(seq(1,8,by=2), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)])),
                                  difftime(t$last_date[9], t$NDZ_sanemsanas_datums[9], units = "days") + 1) 
               } else {stop("Starpkodi9 iztrūkst apstrādes koda.")}
             } else if (t$zinkod[9] %in% c("21", "22", "23", "24", "25", "29")) {
               if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
                 if (t$period[1] == '______' && t$PS_code[1] ==  '______________' && t$NM_code[1] ==  '______________') {
                   yt$dienas <- sum(sapply(seq(1,8,by=2), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)])))
                 } else {stop("Starpkodi9 iztrūkst apstrādes koda.")}
               } else {stop("Starpkodi9 iztrūkst apstrādes koda.")}
             } else {stop("Starpkodi9 iztrūkst apstrādes koda.")}  
  } else if (t$zinkod[1] %in% c("41", "51", "54", "92")) { 
            if (t$zinkod[2] %in% c("40", "50", "53", "91")) {
              if (t$zinkod[3] %in% c("41", "51", "54", "92")) {
                if (t$zinkod[4] %in% c("41", "51", "54", "92") && 
                    t$zinkod[5] %in% c("40", "50", "53", "91") && 
                    t$zinkod[6] %in% c("41", "51", "54", "92") &&  
                    t$zinkod[7] %in% c("40", "50", "53", "91") && 
                    t$zinkod[8] %in% c("41", "51", "54", "92") && 
                    t$zinkod[9] %in% c("21", "22", "23", "24", "25", "29")) {
                  if (all(sapply(c(1:3,5:8), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0)) &&
                      diff(t$NDZ_sanemsanas_datums[4:5]) == 0) {
                    if (t$period[1] == '______' && t$PS_code[1] ==  '______________' && t$NM_code[1] ==  '______________') {
                      t <- t[-4, ]; rownames(t) <- NULL
                      yt$dienas <- sum(sapply(seq(1,8,by=2), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)])) + 1) 
                    } else {stop("Starpkodi9 iztrūkst apstrādes koda.")}
                  } else {stop("Starpkodi9 iztrūkst apstrādes koda.")}
                } else {stop("Starpkodi9 iztrūkst apstrādes koda.")} 
              } else if (t$zinkod[3] %in% c("40", "50", "53", "91")) {
                if (t$zinkod[4] %in% c("41", "51", "54", "92") && 
                    t$zinkod[5] %in% c("40", "50", "53", "91") && 
                    t$zinkod[6] %in% c("41", "51", "54", "92") &&  
                    t$zinkod[7] %in% c("40", "50", "53", "91") && 
                    t$zinkod[8] %in% c("41", "51", "54", "92") && 
                    t$zinkod[9] %in% c("21", "22", "23", "24", "25", "29")) {
                    if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
                    if (t$period[1] == '______' && t$PS_code[1] ==  '______________' && t$NM_code[1] ==  '______________') {
                         yt$dienas <- sum(as.numeric(diff(t$NDZ_sanemsanas_datums[c(1,3)])),
                                          sapply(seq(4,8,by=2), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)])) + 1) 
                    } else {stop("Starpkodi9 iztrūkst apstrādes koda.")}
                  } else {stop("Starpkodi9 iztrūkst apstrādes koda.")}
                } else {stop("Starpkodi9 iztrūkst apstrādes koda.")} 
              }else {stop("Starpkodi9 iztrūkst apstrādes koda.")} 
            } else {stop("Starpkodi9 iztrūkst apstrādes koda.")} 


            
       
  #} else if (t$zinkod[1] == "11" && 
  #           t$zinkod[2] %in% c("40", "50", "53", "91")) {  && 
  #           t$zinkod[3] %in% c("41", "51", "54", "92")) { && 
  #           t$zinkod[4] %in% c("40", "50", "53", "91")) {  && 
  #           t$zinkod[5] %in% c("41", "51", "54", "92")) { && 
  #           t$zinkod[6] %in% c("40", "50", "53", "91")) {  && 
  #           t$zinkod[7] %in% c("41", "51", "54", "92")) { && 
  #           t$zinkod[8] %in% c("40", "50", "53", "91")) {  && 
  #           t$zinkod[9] == "21" && 
  #           all(!diff(t$NDZ_sanemsanas_datums) == 0)) {
  #  days1 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[2], t$NDZ_sanemsanas_datums[1], units = "days"))
  #  days2 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[4], t$NDZ_sanemsanas_datums[3], units = "days"))
  #  days3 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[6], t$NDZ_sanemsanas_datums[5], units = "days"))
  #  days4 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[8], t$NDZ_sanemsanas_datums[7], units = "days"))
#
  #  yt <- y[v, ]
  #  yt$dienas <- sum(days1, days2, days3, days4)
  #  rm(days1, days2, days3, days4)
  } else {stop("Starpkodi9 iztrūkst apstrādes koda.")}
  
  if(is.na(yt$PS_code[1])) {stop("Dienas NA.")}
  yt$zinkod <- "combined"  #jo starpkodu dienu sarēķins
  return(yt)

  }

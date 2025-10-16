starpkodi8 <- function(y, t, prev, v) {

  yt <- y[v, ]
  
  if (all(t$zinkod[1:8] == c("11", "50", "51", "50", "51", "50", "51", "50")) && 
                                          all(diff(t$NDZ_sanemsanas_datums) != 0)) {
                  yt$dienas <- sum(sapply(seq(1,8,by=2), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)])))
  } else if (all(t$zinkod[1:8] == c("21", "11", "50", "51", "50", "51", "50", "51")) && 
             diff(t$NDZ_sanemsanas_datums[4:5]) == 0 &&
             all(sapply(c(1,2,5,6,7), function(i) t$NDZ_sanemsanas_datums[i:(i+1)] != 0))) {
             yt$dienas <- as.numeric(sum(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days"),
                                         diff(t$NDZ_sanemsanas_datums[2:3]), diff(t$NDZ_sanemsanas_datums[4:5]),
                                         diff(t$NDZ_sanemsanas_datums[6:7]), difftime(t$last_date[8], t$NDZ_sanemsanas_datums[8], units = "days")))
  } else if (all(t$zinkod[1:8] == c("11", "53", "54", "53", "54", "53", "54", "53")) && 
             all(diff(t$NDZ_sanemsanas_datums[3:4]) == 0) &&
             all(diff(t$NDZ_sanemsanas_datums[7:8]) == 0) && 
             all(diff(t$NDZ_sanemsanas_datums[1:3]) != 0) &&
             all(diff(t$NDZ_sanemsanas_datums[5:7]) != 0)) {
                yt$dienas <- sum(sapply(seq(1,8,by=2), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)])))
  } else if (all(t$zinkod[1:8] == c("25", "11", "11", "50", "25", "51", "25", "11")) && 
             all(sapply(seq(1, nrow(t), by = 2), function(i) all(diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0))) &&
             t$PS_code[1] ==  '______________' && t$NM_code[1] ==  '______________') {
                yt$dienas <- as.numeric(sum(diff(t$NDZ_sanemsanas_datums[1:2]) + 1, #jo darbs
                                            diff(t$NDZ_sanemsanas_datums[7:8]) + 1)) #jo darbs
  } else if (t$zinkod[1] %in% c("11", "14", "16", "61")) {
            yt <- starpkodi8_11(y, t, prev, v)
  } else if (all(t$zinkod[seq(2,8,by=2)] == "50") && all(t$zinkod[seq(3,8,by=2)] == "51") && t$zinkod[1] == "11") {
            yt$dienas <- sum(sapply(seq(1,8,by=2), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)])))
  } else if (t$zinkod[1] %in% c("40", "50", "53", "91")) {
             starpkodi8_50(y, t, prev, v)
  } else if (t$zinkod[1] %in% c("41", "51", "54", "92")) {
          if (t$zinkod[2] %in% c("40", "50", "53", "91")) {
            if (t$zinkod[3] %in% c("41", "51", "54", "92")) {
              if (t$zinkod[4] %in% c("40", "50", "53", "91")) {
                if (t$zinkod[5] %in% c("41", "51", "54", "92")) {
                  if (t$zinkod[6] %in% c("40", "50", "53", "91")) {
                    if (t$zinkod[7] %in% c("21", "22", "23", "24", "25", "29")) {
                      if (t$zinkod[8] %in% c("41", "51", "54", "92")) {
                        if (diff(t$NDZ_sanemsanas_datums[7:8]) == 0 && all(diff(t$NDZ_sanemsanas_datums[1:7]) != 0)) {
                          if (t$period[1] == '______' && t$PS_code[1] ==  '______________' && t$NM_code[1] ==  '______________') {
                            yt$dienas <- sum(sapply(seq(1,8,by=2), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)])))
                          } else {stop("Starpkodi8 iztrūkst apstrādes koda.")}
                        } else {stop("Starpkodi8 iztrūkst apstrādes koda.")}
                      } else {stop("Starpkodi8 iztrūkst apstrādes koda.")}
                    } else if (t$zinkod[7] %in% c("41", "51", "54", "92")) {
                             if (t$zinkod[8] %in% c("21", "22", "23", "24", "25", "29")) {
                               if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
                                  if (t$period[1] == '______' && t$PS_code[1] ==  '______________' && t$NM_code[1] ==  '______________') {
                                      yt$dienas <- sum(sapply(seq(1,8,by=2), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]))) + 1
                                  } else {stop("Starpkodi8 iztrūkst apstrādes koda.")}
                             } else {stop("Starpkodi8 iztrūkst apstrādes koda.")}
                      } else {stop("Starpkodi8 iztrūkst apstrādes koda.")}
                    } else {stop("Starpkodi8 iztrūkst apstrādes koda.")}
                  } else {stop("Starpkodi8 iztrūkst apstrādes koda.")}
                } else {stop("Starpkodi8 iztrūkst apstrādes koda.")}
              } else {stop("Starpkodi8 iztrūkst apstrādes koda.")}
            } else {stop("Starpkodi8 iztrūkst apstrādes koda.")}
          } else {stop("Starpkodi8 iztrūkst apstrādes koda.")}
  } else {stop("Starpkodi8 iztrūkst apstrādes koda.")}
  
  if(is.na(yt$PS_code[1])) {stop("Dienas NA.")}
  yt$zinkod <- "combined"  #jo starpkodu dienu sarēķins
  return(yt)
}

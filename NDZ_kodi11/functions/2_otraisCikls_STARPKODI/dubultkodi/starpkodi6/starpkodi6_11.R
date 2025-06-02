starpkodi6_11 <- function(y, t, prev, v) {

  yt <- y[v, ]
  
  if (t$zinkod[2] %in% c("41", "51", "54", "92")) {
    if (t$zinkod[3] %in% c("40", "50", "53", "91")) {
      if (t$zinkod[4] %in% c("11", "14", "16", "61")) {
        if (t$zinkod[5] %in% c("40", "50", "53", "91")) {
          if (t$zinkod[6] %in% c("41", "51", "54", "92")) {
            if (all(sapply(c(1,2,4), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
                all(sapply(c(3,5), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
              if (t$period[1] == "______" && t$PS_code[1] == "___________"   && t$NM_code[1] == "___________" ) {
                yt$dienas <- sum(as.numeric(diff(t$NDZ_sanemsanas_datums[4:5])),
                                 as.numeric(difftime(t$last_date[6], t$NDZ_sanemsanas_datums[6], units = "days")) + 1) 
              } else {stop("starpkodi6_11: Trūkst izstrādes koda.")}
            } else {stop("starpkodi6_11: Trūkst izstrādes koda.")} 
          } else {stop("starpkodi6_11: Trūkst izstrādes koda.")} 
        } else {stop("starpkodi6_11: Trūkst izstrādes koda.")} 
      } else {stop("starpkodi6_11: Trūkst izstrādes koda.")} 
    } else {stop("starpkodi6_11: Trūkst izstrādes koda.")}
} else if (t$zinkod[2] %in% c("21", "22", "23", "24", "25", "29") && 
             t$zinkod[3] %in% c("11", "14", "16", "61") && 
             t$zinkod[4] %in% c("40", "50", "53", "91") && 
             t$zinkod[5] %in% c("41", "51", "54", "92") && 
             t$zinkod[6] %in% c("21", "22", "23", "24", "25", "29") && 
             all(diff(t$NDZ_sanemsanas_datums[1:2]) == 0) &&
             all(diff(t$NDZ_sanemsanas_datums[2:6]) != 0)) {
    days1 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[4], t$NDZ_sanemsanas_datums[3], units = "days"))
    days2 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[6], t$NDZ_sanemsanas_datums[5], units = "days")) + 1 
    
    yt$dienas <- sum(days1, days2)
    rm(days1, days2)
} else if (t$zinkod[2] %in% c("40", "50", "53", "91")) {
  yt <- starpkodi6_11_50(y, t, prev, v)
} else if (t$zinkod[2] %in% c("11", "14", "16", "61")) {
  if (all(t$zinkod[seq(3,6,by=2)] == "11") && all(t$zinkod[c(2,4)] == "25") && t$zinkod[6] == "50" && 
      all(diff(t$NDZ_sanemsanas_datums) != 0)) {
    yt$dienas <- sum(sapply(seq(1,6,by=2), function(i) as.numeric(difftime(t$NDZ_sanemsanas_datums[i+1], t$NDZ_sanemsanas_datums[i], units = "days")) + 1)) - 1 
  } else if (t$zinkod[3] %in% c("40", "50", "53", "91")) {
              if (t$zinkod[4] %in% c("41", "51", "54", "92")) {
                if (t$zinkod[5] %in% c("40", "50", "53", "91")) {
                  if (t$zinkod[6] %in% c("21", "22", "23", "24", "25", "29")) {
                    if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
                      if (t$period[1] == "______"  && t$PS_code[1] == "___________"   && t$NM_code[1] == "___________" ) {
                        yt$dienas <- sum(sapply(c(2,4), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)])))
                      } else {stop("starpkodi6_11: Trūkst izstrādes koda.")}
                    } else {stop("starpkodi6_11: Trūkst izstrādes koda.")}
                  } else {stop("starpkodi6_11: Trūkst izstrādes koda.")}
                } else {stop("starpkodi6_11: Trūkst izstrādes koda.")}
              } else if (t$zinkod[4] %in% c("40", "50", "53", "91")) {
                if (t$zinkod[5] %in% c("41", "51", "54", "92")) {
                  if (t$zinkod[6] %in% c("41", "51", "54", "92")) {
                    if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
                      if ((t$period[1] == "______"  && t$PS_code[1] == "___________"   && t$NM_code[1] == "___________" ) ||
                          (t$period[1] == "______"  && t$PS_code[1] == "___________"   && t$NM_code[1] == "___________" )) {
                        yt$dienas <- sum(as.numeric(diff(t$NDZ_sanemsanas_datums[c(2,4)])),
                                         as.numeric(difftime(t$last_date[6], t$NDZ_sanemsanas_datums[6], units = "days"))) + 1
                      } else {stop("starpkodi6_11: Trūkst izstrādes koda.")}
                    } else {stop("starpkodi6_11: Trūkst izstrādes koda.")}
                  } else {stop("starpkodi6_11: Trūkst izstrādes koda.")}
                } else {stop("starpkodi6_11: Trūkst izstrādes koda.")}
              } else {stop("starpkodi6_11: Trūkst izstrādes koda.")}
            } else {stop("starpkodi6_11: Trūkst izstrādes koda.")}     
} else {stop("starpkodi6_11: Trūkst izstrādes koda.")}

  rm(y, t, prev, v)  
  return(yt)
}

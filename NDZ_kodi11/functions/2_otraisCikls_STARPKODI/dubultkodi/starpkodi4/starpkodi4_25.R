starpkodi4_25 <- function(y, t, prev, v) {
  
  yt <- y[v, ]
  
  if (t$zinkod[2] %in% c("41", "51", "54", "92")) {
           if (t$zinkod[3] %in% c("11", "14", "16", "61")) {
             if (t$zinkod[4] %in% c("21", "22", "23", "24", "25", "29")) {
               if (diff(t$NDZ_sanemsanas_datums[1:2]) == 0 && diff(t$NDZ_sanemsanas_datums[3:4]) != 0) {
                 yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[3:4]))
               } else {stop("starpkodi4_25: Iztrūkst aptrādes koda.")}
             } else if (t$zinkod[4] %in% c("40", "50", "53", "91")) {
               if (diff(t$NDZ_sanemsanas_datums[1:2]) == 0 && diff(t$NDZ_sanemsanas_datums[3:4]) != 0) {
                 if (t$period[1] == '______' && t$PS_code[1] ==  '______________' && t$NM_code[1] ==  '______________') {
                   yt$dienas <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) + 1, 
                                    as.numeric(diff(t$NDZ_sanemsanas_datums[3:4])))
                 } else {stop("starpkodi4_25: Iztrūkst aptrādes koda.")}
               } else {stop("1starpkodi4_25: Iztrūkst aptrādes koda.")}
             } else {stop("2starpkodi4_25: Iztrūkst aptrādes koda.")}
           } else if (t$zinkod[3] %in% c("41", "51", "54", "92")) {
                    if (t$zinkod[4] %in% c("41", "51", "54", "92")) {
                       #    yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days"))
                     #} else 
                         if (all(sapply(c(1,3), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) && 
                                diff(t$NDZ_sanemsanas_datums[2:3]) != 0) {
                               if (t$period[1] == '______' && t$PS_code[1] ==  '______________' && t$NM_code[1] ==  '______________') {
                                   yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[1:2]))
                                   ZERO_minus(t %>% slice(2))
                               } else {stop("starpkodi4_25: Iztrūkst aptrādes koda.")}
                       } else {stop("starpkodi4_25: Iztrūkst aptrādes koda.")}
                } else if (t$zinkod[4] %in% c("40", "50", "53", "91")) {
                        if (all(diff(t$NDZ_sanemsanas_datums[2:4]) != 0) && diff(t$NDZ_sanemsanas_datums[1:2]) == 0) {
                                 if (t$period[1] == '______' && t$PS_code[1] ==  '______________' && t$NM_code[1] ==  '______________') {
                                     t <- t[c(2,1,3,4),]; rownames(t) <- NULL
                                     yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[1:2]))
                                } else {stop("starpkodi4_25: Iztrūkst aptrādes koda.")}
                            } else {stop("starpkodi4_25: Iztrūkst aptrādes koda.")}
                } else if (t$zinkod[4] %in% c("11", "14", "16", "61")) {
                         if (all(diff(t$NDZ_sanemsanas_datums[2:4]) != 0) && diff(t$NDZ_sanemsanas_datums[1:2]) == 0) {
                            if (t$period[1] == '______' && t$PS_code[1] ==  '______________' && t$NM_code[1] ==  '______________') {
                                t <- t[c(2,1,3,4),]; rownames(t) <- NULL
                                yt$dienas <- sum(as.numeric(diff(t$NDZ_sanemsanas_datums[1:2])),
                                                 as.numeric(difftime(t$last_date[4], t$NDZ_sanemsanas_datums[4], units = "days"))) + 1
                    } else {stop("starpkodi4_25: Iztrūkst aptrādes koda.")}
                  } else {stop("starpkodi4_25: Iztrūkst aptrādes koda.")}
                } else {stop("starpkodi4_25: Iztrūkst aptrādes koda.")}
           } else if (t$zinkod[3] %in% c("40", "50", "53", "91")) {
                    if (t$zinkod[4] %in% c("41", "51", "54", "92")) {
                      if (all(diff(t$NDZ_sanemsanas_datums[2:4]) != 0) && diff(t$NDZ_sanemsanas_datums[1:2]) == 0) {
                          if (t$period[1] == '______' && t$PS_code[1] ==  '______________' && t$NM_code[1] ==  '______________') {
                               yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[1:2]))
                          } else {stop("starpkodi4_25: Iztrūkst aptrādes koda.")}
                      } else if (all(diff(t$NDZ_sanemsanas_datums[1:3]) == 0) && diff(t$NDZ_sanemsanas_datums[3:4]) != 0) {
                                 if (t$period[1] == '______' && t$PS_code[1] ==  '______________' && t$NM_code[1] ==  '______________') {
                                      yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days"))
                                 } else {stop("8starpkodi4_25: Iztrūkst aptrādes koda.")}
                      } else {stop("7starpkodi4_25: Iztrūkst aptrādes koda.")}
             } else {stop("8starpkodi4_25: Iztrūkst aptrādes koda.")}
           } else {stop("9starpkodi4_25: Iztrūkst aptrādes koda.")}
  } else if (t$zinkod[2] %in% c("11", "14", "16", "61")) {
          if (t$zinkod[3] %in% c("40", "50", "53", "91")) {
            if (t$zinkod[4] %in% c("41", "51", "54", "92")) {
              if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
                yt$dienas <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")),
                                 as.numeric(diff(t$NDZ_sanemsanas_datums[2:3])),
                                 as.numeric(difftime(t$last_date[4], t$NDZ_sanemsanas_datums[4], units = "days")) + 1)
              } else {stop("starpkodi4_25: Iztrūkst aptrādes koda.")}
            } else if (t$zinkod[4] %in% c("21", "22", "23", "24", "25", "29")) {
              if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
                if (t$period[1] == '______' && t$PS_code[1] ==  '______________' && t$NM_code[1] ==  '______________') {
                  yt$dienas <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")),
                                   as.numeric(diff(t$NDZ_sanemsanas_datums[2:3]))) 
                } else {stop("starpkodi4_25: Iztrūkst aptrādes koda.")}
              } else {stop("starpkodi4_25: Iztrūkst aptrādes koda.")}
            } else {stop("starpkodi4_25: Iztrūkst aptrādes koda.")}
        } else if (t$zinkod[3] == "11") {
                if (t$zinkod[4] %in% c("40", "50", "53", "91")) {
                  if (all(sapply(c(1,3), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
                      diff(t$NDZ_sanemsanas_datums[2:3]) != 0) {
                      if (t$PS_code[1] ==  '______________' && t$NM_code[1] ==  '______________') {
                        yt$dienas <- as.numeric(sum(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days"),
                                          diff(t$NDZ_sanemsanas_datums[3:4])))
                     } else {stop("starpkodi4_25: Iztrūkst aptrādes koda.")}
                  } else {stop("starpkodi4_25: Iztrūkst aptrādes koda.")}
              } else {stop("starpkodi4_25: Iztrūkst aptrādes koda.")}
          } else {stop("starpkodi4_25: Iztrūkst aptrādes koda.")}
    } else {stop("starpkodi4_25: Iztrūkst aptrādes koda.")}

  rm(y, t, prev, v)
  return(yt)
} 

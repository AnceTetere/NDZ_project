starpkodi3_11_50 <- function(y, t, prev, v) {

  yt <- y[v, ]
  
  if (t$zinkod[3] %in% c("41", "51", "54", "92")) {
          yt$dienas <- sum(as.numeric(diff(t$NDZ_sanemsanas_datums[1:2])), 
                           as.numeric(difftime(t$last_date[3], t$NDZ_sanemsanas_datums[3], units = "days")) + 1)
  } else if (t$zinkod[3] %in% c("21", "22", "23", "24", "25", "29")) {
        if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
          yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[1:2]))
       } else if (diff(t$NDZ_sanemsanas_datums[1:2]) == 0 && diff(t$NDZ_sanemsanas_datums[2:3]) != 0) {
          yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[1:2]))
        } else {stop("Starpkodi3_11_50: Trūkst izstrādes koda.")}
  } else if (t$zinkod[3] %in% c("40", "50", "53", "91")) {
          if (all(diff(t$NDZ_sanemsanas_datums) == 0)) {
            if ((NDZ$period[1] == '______' && t$PS_code[1] ==  '______________' && t$NM_code[1] ==  '______________') ||
                (NDZ$period[1] == '______' && t$PS_code[1] ==  '______________' && t$NM_code[1] ==  '______________') ||
                (NDZ$period[1] == '______' && t$PS_code[1] ==  '______________' && t$NM_code[1] ==  '______________') ||
                (NDZ$period[1] == '______' && t$PS_code[1] ==  '______________' && t$NM_code[1] ==  '______________') ||
                (NDZ$period[1] == '______' && t$PS_code[1] ==  '______________' && t$NM_code[1] ==  '______________') ||
                (NDZ$period[1] == '______' && t$PS_code[1] ==  '______________' && t$NM_code[1] ==  '______________')) {
              yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[1:2]))
            } else {stop("Starpkodi3_11_50: Trūkst izstrādes koda.")}
          } else if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
            yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[3], t$NDZ_sanemsanas_datums[1], units = "days"))
          } else if (diff(t$NDZ_sanemsanas_datums[1:2]) == 0 && diff(t$NDZ_sanemsanas_datums[2:3]) != 0) {
            if ((t$PS_code ==  '______________' && t$NM_code[1] ==  '______________') ||
                (t$PS_code ==  '______________' && t$NM_code[1] ==  '______________') ||
                (NDZ$period[1] == '______' && t$PS_code ==  '______________' && t$NM_code[1] ==  '______________') ||
                (NDZ$period[1] == '______' && t$PS_code ==  '______________' && t$NM_code[1] ==  '______________')) {
              yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[c(1,3)]))
            } else {stop("Starpkodi3_11_50: Trūkst izstrādes koda.")}
          } else {stop("Starpkodi3_11_50: Trūkst izstrādes koda.")}
  } else if (t$zinkod[3] %in% c("11", "14", "16", "61")) {
           if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
             if ((t$period[1] == '______' && t$PS_code ==  '______________' && t$NM_code[1] ==  '______________') ||
                 (t$period[1] == '______' && t$PS_code ==  '______________' && t$NM_code[1] ==  '______________')) {
               yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[1:2]))
             } else {stop("Starpkodi3_11_50: Trūkst izstrādes koda.")}
           } else if (diff(t$NDZ_sanemsanas_datums[1:2]) == 0 && diff(t$NDZ_sanemsanas_datums[2:3]) != 0) {
             if (t$period[1] == '______' && t$PS_code ==  '______________' && t$NM_code[1] ==  '______________') {
               yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[1:2]))
             } else {stop("Starpkodi3_11_50: Trūkst izstrādes koda.")}
           } else {stop("Starpkodi3_11_50: Trūkst izstrādes koda.")}
  } else {stop("Starpkodi3_11_50: Trūkst izstrādes koda.")}
  
  rm(y, t, prev, v)
  return(yt) 
}  

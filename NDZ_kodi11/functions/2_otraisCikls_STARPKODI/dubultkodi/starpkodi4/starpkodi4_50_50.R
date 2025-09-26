starpkodi4_50_50 <- function(y, t, prev, v) {
  
  yt <- y[v,]
  
  if (t$zinkod[3] %in% c("41", "51", "54", "92")) {
    yt <- starpkodi4_50_50_51(y, t, prev, v)
  } else if (t$zinkod[3] %in% c("40", "50", "53", "91")) {
             if (t$zinkod[4] %in% c("41", "51", "54", "92")) {
               if (all(sapply(c(1,3), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) && diff(t$NDZ_sanemsanas_datums[2:3]) != 0) {
                         if (t$period[1] == '______' && t$PS_code[1] ==  '______________' && t$NM_code[1] ==  '______________') {
                           yt$dienas <- as.numeric(sum(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days") - 1,
                                                       diff(t$NDZ_sanemsanas_datums[3:4]) + 1))
                         } else {stop("starpkodi4_50_50: Trūkst izstrādes koda.")}
               } else if (diff(t$NDZ_sanemsanas_datums[3:4]) == 0 && all(diff(t$NDZ_sanemsanas_datums[1:3]) != 0)) {
                           if (t$period[1] == '______' && t$PS_code[1] ==  '______________' && t$NM_code[1] ==  '______________') {
                               t <- t[c(1,2,4,3),]; rownames(t) <- NULL
                              yt$dienas <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[2], prev, units = "days")) - 1,
                                               as.numeric(diff(t$NDZ_sanemsanas_datums[3:4])))
                           } else {stop("starpkodi4_50_50: Trūkst izstrādes koda.")}
               } else {stop("starpkodi4_50_50: Trūkst izstrādes koda.")}  
             } else {stop("starpkodi4_50_50: Trūkst izstrādes koda.")}
  } else if (t$zinkod[3] %in% c("21", "22", "23", "24", "25", "29")) {
           if (t$zinkod[4] %in% c("41", "51", "54", "92")) {
             if (all(diff(t$NDZ_sanemsanas_datums[1:3]) != 0) && diff(t$NDZ_sanemsanas_datums[3:4]) == 0) {
                        t <- t[c(1,2,4,3),]; rownames(t) <- NULL
                        yt$dienas <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[2], prev, units = "days")) - 1,
                                         as.numeric(diff(t$NDZ_sanemsanas_datums[3:4])))
            } else {stop("starpkodi4_50_50: Trūkst izstrādes koda.")}  
        } else {stop("starpkodi4_50_50: Trūkst izstrādes koda.")}
    } else {stop("starpkodi4_50_50: Trūkst izstrādes koda.")}    
  
  
  rm(y, t, prev, v)
  return(yt)
}

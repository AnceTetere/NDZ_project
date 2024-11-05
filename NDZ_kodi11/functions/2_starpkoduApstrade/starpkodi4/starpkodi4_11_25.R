starpkodi4_11_25 <- function(y, t, prev, v) {
  
  if (t$zinkod[3] %in% c("11", "16", "61")) {
           if (t$zinkod[4] %in% c("40", "50", "53", "91")) {
             if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
               yt <- y[v, ]
               yt$dienas <- sum(as.numeric(diff(t$NDZ_sanemsanas_datums[1:2])) + 1, #jo atlaišana 
                                as.numeric(diff(t$NDZ_sanemsanas_datums[3:4])))
               
             } else if (all(sapply(seq(1,4,by=2), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
                        diff(t$NDZ_sanemsanas_datums[2:3]) != 0) {
               yt <- y[v, ]
               yt$dienas <- sum(as.numeric(diff(t$NDZ_sanemsanas_datums[1:2])), 
                                as.numeric(diff(t$NDZ_sanemsanas_datums[3:4])))
             } else {stop("Starpkodi4_11_25: Trūkst izstrādes koda.")}
           } else {stop("Starpkodi4_11_25: Trūkst izstrādes koda.")}
  } else if (t$zinkod[3] %in% c("40", "50", "53", "91")) {
            if (t$zinkod[4] %in% c("11", "16", "61")) {
              if (all(diff(t$NDZ_sanemsanas_datums[1:3]) == 0) && diff(t$NDZ_sanemsanas_datums[3:4]) != 0) {
                yt <- y[v, ]
                yt$dienas <- as.numeric(difftime(t$last_date[4], t$NDZ_sanemsanas_datums[4], units = "days")) + 1
              } else {stop("Starpkodi4_11_25: Trūkst izstrādes koda.")}
            } else {stop("Starpkodi4_11_25: Trūkst izstrādes koda.")}
  } else {stop("Starpkodi4_11_25: Trūkst izstrādes koda.")}
  
  rm(y, t, prev, v)
  return(yt)
}

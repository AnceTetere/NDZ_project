starpkodi4_11_50_51 <- function(y, t, prev, v) {
  
  if (t$zinkod[4] %in% c("40", "50", "53", "91")) {
             if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
               yt <- y[v, ]
               yt$dienas <- sum(sapply(c(1,3), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)])))
             } else if (all(diff(t$NDZ_sanemsanas_datums[2:4]) != 0) && diff(t$NDZ_sanemsanas_datums[1:2]) == 0) {
               yt <- y[v, ]
               yt$dienas <- sum(sapply(c(1,3), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)])), 1)
             } else {stop("Starpkodi4_11_50_51: Trūkst izstrādes koda.")}
  } else if (t$zinkod[4] %in% c("41", "51", "54", "92")) {
           if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
             yt <- y[v, ]
             yt$dienas <- sum(as.numeric(diff(t$NDZ_sanemsanas_datums[1:2])),
                              as.numeric(difftime(t$last_date[4], t$NDZ_sanemsanas_datums[4], units = "days")), 1)
             
        #  } else if (diff(t$NDZ_sanemsanas_datums[2:3]) == 0 && all(sapply(c(1,3), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
        #      yt <- y[v, ]
        #      yt$dienas <- sum(as.numeric(diff(t$NDZ_sanemsanas_datums[1:2])),
        #                       as.numeric(difftime(t$last_date[4], t$NDZ_sanemsanas_datums[4], units = "days")) + 1)
        #     
         #  } else if (all(diff(t$NDZ_sanemsanas_datums[1:3]) != 0) && diff(t$NDZ_sanemsanas_datums[3:4]) == 0) {
         #        yt <- y[v, ]
         #        yt$dienas <- sum(as.numeric(diff(t$NDZ_sanemsanas_datums[1:2])),
         #                         as.numeric(difftime(t$last_date[4], t$NDZ_sanemsanas_datums[4], units = "days")) + 1)
             } else {stop("Starpkodi4_11_50_51: Trūkst izstrādes koda.")}
  } else if(t$zinkod[4] %in% c("21", "22", "23", "25", "24", "29")) {
    if (diff(t$NDZ_sanemsanas_datums[1:2]) == 0 && diff(t$NDZ_sanemsanas_datums[3:4]) != 0) {
      yt <- y[v, ]
      yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[3:4]))
    } else if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
      yt <- y[v, ]
      yt$dienas <- sum(sapply(c(1,3), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)])))
    } else if (all(sapply(c(1,3), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0)) && diff(t$NDZ_sanemsanas_datums[2:3]) == 0) {
      yt <- y[v, ]
      yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[c(1,4)])) 
    } else {stop("Starpkodi4_11_50_51: Trūkst izstrādes koda.")}
  } else {stop("Starpkodi4_11_50_51: Trūkst izstrādes koda.")}
  
  rm(y, t, prev, v)
  return(yt) 
}

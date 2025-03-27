starpkodi5_50_51_50 <- function(y, t, prev, v) {
  
  yt <- y[v, ]
  
  if (t$zinkod[4] %in% c("41", "51", "54", "92")) {
    yt <- starpkodi5_50_51_50_51(y, t, prev, v)
  } else if (t$zinkod[4] %in% c("21", "22", "23", "24", "25", "29")) {
             if (t$zinkod[5] %in% c("41", "51", "54", "92")) {
               if (all(sapply(c(1,4), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) && 
                   all(diff(t$NDZ_sanemsanas_datums[2:4]) != 0)) {
                 yt$dienas <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1
                                  as.numeric(diff(t$NDZ_sanemsanas_datums[2:3])))
               } else if (all(diff(t$NDZ_sanemsanas_datums[1:4]) != 0) && diff(t$NDZ_sanemsanas_datums[4:5]) == 0) {
                 yt$dienas <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1
                                  as.numeric(diff(t$NDZ_sanemsanas_datums[2:3])))
               } else if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
                 if ((t$period[1] == "_____" && t$PS_code[1] == "_____" && t$NM_code[1] == "_____") ||
                     (t$period[1] == "_____" && t$PS_code[1] == "_____" && t$NM_code[1] == "_____")) {
                       yt$dienas <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1
                                        as.numeric(diff(t$NDZ_sanemsanas_datums[2:3])))
                 } else {stop("Starpkodi5_50_51: iztrūkst apstrādes koda.")}
               } else if (all(sapply(c(2,4), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) && 
                          all(sapply(c(1,3), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) { 
                 if (t$period[1] == "_____" && t$PS_code[1] == "__________" && t$NM_code[1] == "__________") {
                   t <- t[c(1,2,3,5,4),]
                   rownames(t) <- NULL
                   yt$dienas <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1
                                    sapply(c(2,4), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)])))
                 } else {stop("Starpkodi5_50_51: iztrūkst apstrādes koda.")}
               } else {stop("Starpkodi5_50_51: iztrūkst apstrādes koda.")}
             } else if (t$zinkod[5] %in% c("40", "50", "53", "91")) {
               if (all(sapply(c(2,4), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) && 
                   all(sapply(c(1,3), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
                 if (t$period[1] == "_____" && t$PS_code[1] == "__________" && t$NM_code[1] == "__________") {
                   yt$dienas <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1
                                    as.numeric(diff(t$NDZ_sanemsanas_datums[c(2,4)])))
                 } else {stop("Starpkodi5_50_51: iztrūkst apstrādes koda.")}
               } else {stop("Starpkodi5_50_51: iztrūkst apstrādes koda.")}
             }  else {stop("Starpkodi5_50_51: iztrūkst apstrādes koda.")}
  } else if(t$zinkod[4] == "51") {
    if(t$zinkod[5] == "25") {
      if(all(diff(t$NDZ_sanemsanas_datums) != 0)) {
        yt$dienas <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1,
                         as.numeric(diff(t$NDZ_sanemsanas_datums[2:3])),
                         as.numeric(diff(t$NDZ_sanemsanas_datums[4:5])))
      } else {stop("Starpkodi5_50_51: iztrūkst apstrādes koda.")}
    } else {stop("Starpkodi5_50_51: iztrūkst apstrādes koda.")}
  } else if(t$zinkod[4] == "21") {
    if(t$zinkod[5] == "51") {
      if(all(diff(t$NDZ_sanemsanas_datums[1:4]) != 0) && diff(t$NDZ_sanemsanas_datums[4:5]) == 0) {
        yt$dienas <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1
                         as.numeric(diff(t$NDZ_sanemsanas_datums[2:3])))
      } else {stop("Starpkodi5_50_51: iztrūkst apstrādes koda.")}
    } else {stop("Starpkodi5_50_51: iztrūkst apstrādes koda.")}
  } else {stop("Starpkodi5_50_51: iztrūkst apstrādes koda.")}
  
  rm(y, t, prev, v)
  return(yt)
}

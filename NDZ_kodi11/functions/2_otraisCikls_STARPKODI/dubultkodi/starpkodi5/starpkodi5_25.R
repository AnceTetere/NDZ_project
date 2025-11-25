starpkodi5_25 <- function(y, t, prev, v) {
  
  yt <- y[v,]
  
  if (t$zinkod[2] %in% c("11", "14", "16", "61")) {
    if (t$zinkod[3] %in% c("40", "50", "53", "91")) {
      if (t$zinkod[4] %in% c("21", "22", "23", "24", "25", "29")) {
        if (t$zinkod[5] %in% c("41", "51", "54", "92")) {
          if (all(sapply(c(2,4), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
              all(sapply(c(1,3), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
              if (t$period[1] == '______' && t$PS_code[1] ==  '______________' && t$NM_code[1] ==  '______________') {
                  t <- t[c(1,2,3,5,4),]; rownames(t) <- NULL 
                  yt$dienas <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")),
                                   sapply(c(2,4), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)])))
              } else {stop("starpkodi5_51: Iztrūkst apstrādes koda.")}
            } else {stop("starpkodi5_51: Iztrūkst apstrādes koda.")}
          } else {stop("starpkodi5_51: Iztrūkst apstrādes koda.")}
        } else {stop("starpkodi5_51: Iztrūkst apstrādes koda.")}
      } else {stop("starpkodi5_51: Iztrūkst apstrādes koda.")}
  } else if (t$zinkod[2] %in% c("41", "51", "54", "92")) {
           if (t$zinkod[3]  %in% c("11", "14", "16", "61")) {
             if (t$zinkod[4] %in% c("21", "22", "23", "24", "25", "29")) {
               if (t$zinkod[5] %in% c("11", "14", "16", "61")) {
                 if (diff(t$NDZ_sanemsanas_datums[1:2]) == 0 && all(diff(t$NDZ_sanemsanas_datums[2:5]) != 0)) {
                     if ((t$period[1] == '______' && t$PS_code[1] ==  '______________' && t$NM_code[1] ==  '______________') ||
                         (t$period[1] == '______' && t$PS_code[1] ==  '______________' && t$NM_code[1] ==  '______________')) {
                         t <- t[c(2,1,3,4,5),]; rownames(t) <- NULL 
                         yt$dienas <- sum(sapply(c(1,3), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)])),
                                      as.numeric(difftime(t$last_date[5], t$NDZ_sanemsanas_datums[5], units = "days"))) + 1
            } else {stop("starpkodi5_51: Iztrūkst apstrādes koda.")}
          } else {stop("starpkodi5_51: Iztrūkst apstrādes koda.")}
        } else {stop("starpkodi5_51: Iztrūkst apstrādes koda.")}
      } else {stop("starpkodi5_51: Iztrūkst apstrādes koda.")}
    } else {stop("starpkodi5_51: Iztrūkst apstrādes koda.")}
  } else {stop("starpkodi5_51: Iztrūkst apstrādes koda.")}
        
  
  rm(y, t, prev, v)
  return(yt)
}

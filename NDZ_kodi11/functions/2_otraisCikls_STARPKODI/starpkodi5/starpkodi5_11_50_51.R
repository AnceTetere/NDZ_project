starpkodi5_11_50_51 <- function(y, t, prev, v) {
  
  yt <- y[v,]
  
  if (t$zinkod[4] %in% c("21", "22", "23", "24", "25", "29")) {
        if (t$zinkod[5] %in% c("41", "51", "54", "92")) {
          if (all(sapply(c(1,4), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) && 
                                             all(diff(t$NDZ_sanemsanas_datums[2:4]) != 0)) {
            if (t$period[1] == "_____" && t$PS_code[1] == '_______' && t$NM_code[1] == '__________') {
              yt$dienas <- sum(as.numeric(diff(t$NDZ_sanemsanas_datums[1:2])),
                               as.numeric(diff(t$NDZ_sanemsanas_datums[4:5])))
            } else {stop("Starpkodi5_11_50_51: Trūkst izstrādes koda.")}
          } else {stop("Starpkodi5_11_50_51: Trūkst izstrādes koda.")}
        } else {stop("Starpkodi5_11_50: Trūkst izstrādes koda.")}  
  } else if (t$zinkod[4] %in% c("40", "50", "53", "91")) {
        if (t$zinkod[5] %in% c("41", "51", "54", "92")) {
          if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
            yt$dienas <- sum(as.numeric(diff(t$NDZ_sanemsanas_datums[1:2])),
                             as.numeric(diff(t$NDZ_sanemsanas_datums[3:4])),
                             as.numeric(difftime(t$last_date[5], t$NDZ_sanemsanas_datums[5], units = "days")))
        
            } else if (all(diff(t$NDZ_sanemsanas_datums[2:5]) != 0) && diff(t$NDZ_sanemsanas_datums[1:2]) == 0) {
            yt$dienas <- sum(as.numeric(diff(t$NDZ_sanemsanas_datums[1:2])),
                             as.numeric(diff(t$NDZ_sanemsanas_datums[3:4])),
                             as.numeric(difftime(t$last_date[5], t$NDZ_sanemsanas_datums[5], units = "days")) + 1) 
            } else if (all(sapply(c(1,3,4), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0)) && 
                       diff(t$NDZ_sanemsanas_datums[2:3]) == 0) {
              yt$dienas <- sum(as.numeric(diff(t$NDZ_sanemsanas_datums[1:2])),
                               as.numeric(diff(t$NDZ_sanemsanas_datums[3:4])),
                               as.numeric(difftime(t$last_date[5], t$NDZ_sanemsanas_datums[5], units = "days")) + 1) 
            } else {stop("Starpkodi5_11_50_51: Trūkst izstrādes koda.")}
        } else if (t$zinkod[5] %in% c("21", "22", "23", "24", "25", "29")) {
            if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
              if (t$period[1] == "_____" && t$PS_code[1] == "__________" && t$NM_code[1] == "__________") {
                yt$dienas <- sum(sapply(c(1,3), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)])))
              } else {stop("Starpkodi5_11_50: Trūkst izstrādes koda.")}
            } else {stop("Starpkodi5_11_50: Trūkst izstrādes koda.")}
        } else {stop("Starpkodi5_11_50: Trūkst izstrādes koda.")}  
  } else if (t$zinkod[4] %in% c("41", "51", "54", "92")) {
    if (t$zinkod[5] %in% c("21", "22", "23", "24", "25", "29")) {
      if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
        yt$dienas <- sum(as.numeric(diff(t$NDZ_sanemsanas_datums[1:2])),
                         as.numeric(diff(t$NDZ_sanemsanas_datums[4:5]) + 1))
      } else {stop("Starpkodi5_11_50_51: Trūkst izstrādes koda.")}
    } else if (t$zinkod[5] %in% c("40", "50", "53", "91")) {
      if (all(diff(t$NDZ_sanemsanas_datums[2:5]) != 0) && diff(t$NDZ_sanemsanas_datums[1:2]) == 0) {
        yt$dienas <- sum(as.numeric(diff(t$NDZ_sanemsanas_datums[1:2])),
                         as.numeric(diff(t$NDZ_sanemsanas_datums[4:5])))
      } else if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
        yt$dienas <- sum(sapply(c(1,4), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)])))
      } else {stop("Starpkodi5_11_50_51: Trūkst izstrādes koda.")}
    } else {stop("Starpkodi5_11_50: Trūkst izstrādes koda.")}  
  } else {stop("Starpkodi5_11_50: Trūkst izstrādes koda.")}
  
      # } else if (t$zinkod[4] == "51") {
      #   if (t$zinkod[5] == "51") {
      #     if (all(sapply(c(2,4), function(i) all(diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0))) &&
      #         all(sapply(c(1,3), function(i) all(diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0)))) {
      #       days1 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[2], t$NDZ_sanemsanas_datums[1], units = "days"))
      #       days2 <- as.numeric(difftime(t$last_date[5], t$NDZ_sanemsanas_datums[5], units = "days")) + 1 
      #       
      #       yt <- y[v, ]
      #       yt$dienas <- sum(days1, days2)
      #       rm(days1, days2)
      #     } else {stop("Starpkodi5_11_50: Trūkst izstrādes koda.")}
      #   } else if (t$zinkod[5] == "25") {
      #     if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
      #       days1 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[3], t$NDZ_sanemsanas_datums[1], units = "days"))
      #       days2 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[5], t$NDZ_sanemsanas_datums[4], units = "days")) + 1
      #       
      #       yt <- y[v, ]
      #       yt$dienas <- sum(days1, days2)
      #       rm(days1, days2)
      #     } else {stop("Starpkodi5_11_50: Trūkst izstrādes koda.")}
    
  
  rm(y, t, prev, v)
  return(yt)

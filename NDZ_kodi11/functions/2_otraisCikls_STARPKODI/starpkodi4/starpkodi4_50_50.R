starpkodi4_50_50 <- function(y, t, prev, v) {
  
  yt <- y[v,]
  
  if (t$zinkod[3] %in% c("41", "51", "54", "92")) {
    if (t$zinkod[4] %in% c("21", "22", "23", "24", "25", "29")) {
      if (diff(t$NDZ_sanemsanas_datums[2:3]) == 0 && all(sapply(c(1,3), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
        if (t$PS_code[1] == '__________' && t$NM_code[1] == '__________') {
          yt$dienas <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[2], prev, units = "days")) - 1, 
                           as.numeric(diff(t$NDZ_sanemsanas_datums[3:4])) + 1) 
        } else {stop("starpkodi4_50_50: Trūkst izstrādes koda.")}
      } else {stop("starpkodi4_50_50: Trūkst izstrādes koda.")}
    } else if (t$zinkod[4] %in% c("40", "50", "53", "91")) {
      if (diff(t$NDZ_sanemsanas_datums[1:2]) == 0 && all(diff(t$NDZ_sanemsanas_datums[2:4]) != 0)) {
        if(t$PS_code[1] == '__________' && t$NM_code[1] == '__________') {
          yt$dienas <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[2], prev, units = "days")) - 1, 
                           as.numeric(diff(t$NDZ_sanemsanas_datums[3:4])))
        } else {stop("starpkodi4_50_50: Trūkst izstrādes koda.")}
      } else if (diff(t$NDZ_sanemsanas_datums[3:4]) == 0 && all(diff(t$NDZ_sanemsanas_datums[1:3]) != 0)) {
        if(t$period[1] == "__________" && t$PS_code[1] == '__________' && t$NM_code[1] == '__________') {
          yt$dienas <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[2], prev, units = "days")) - 1, 
                           as.numeric(diff(t$NDZ_sanemsanas_datums[3:4])))
          ZERO_plus(t[4,])
        } else {stop("starpkodi4_50_50: Trūkst izstrādes koda.")}
      } else {stop("starpkodi4_50_50: Trūkst izstrādes koda.")}
    } else {stop("starpkodi4_50_50: Trūkst izstrādes koda.")} 
    # } else if(t$zinkod[3] == "51") {
    #   if(t$zinkod[4] == "21"){
    #     if(diff(t$NDZ_sanemsanas_datums[1:2]) == 0 && all(diff(t$NDZ_sanemsanas_datums[2:4]) != 0)) {
    #       days1 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1 # jo atvaļinājums
    #       days2 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[4], t$NDZ_sanemsanas_datums[3], units = "days")) + 1 
    #       
    #       yt$dienas <- sum(days1, days2)
    #       rm(days1, days2)
    #     } else {stop("starpkodi4_50_50: Trūkst izstrādes koda.")}
    #   } else {stop("starpkodi4_50_50: Trūkst izstrādes koda.")}
    # } else if (t$zinkod[3] == "25") {
    #   if (t$zinkod[4] == "51") {
    #     if (diff(t$NDZ_sanemsanas_datums[3:4]) == 0 && all(diff(t$NDZ_sanemsanas_datums[1:3]) != 0)) {
    #       yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[2], prev, units = "days")) - 1
    #     } else {stop("starpkodi4_50_50: Trūkst izstrādes koda.")}
    #   } else {stop("starpkodi4_50_50: Trūkst izstrādes koda.")}
  } else if (t$zinkod[3] %in% c("40", "50", "53", "91")) {
    if (t$zinkod[4] %in% c("41", "51", "54", "92")) {
      if(all(sapply(c(1,3), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) && diff(t$NDZ_sanemsanas_datums[2:3]) != 0) {
        if (t$period[1] == "__________" && t$PS_code[1] == '__________' && t$NM_code[1] == '__________') {
          yt$dienas <- as.numeric(sum(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days") - 1,
                                      diff(t$NDZ_sanemsanas_datums[3:4]) + 1))
        } else {stop("starpkodi4_50_50: Trūkst izstrādes koda.")}
      } else {stop("starpkodi4_50_50: Trūkst izstrādes koda.")}  
    } else {stop("starpkodi4_50_50: Trūkst izstrādes koda.")}
  } else if (t$zinkod[3] %in% c("21", "22", "23", "24", "25", "29")) {
    if (t$zinkod[4] %in% c("41", "51", "54", "92")) {
      if(all(diff(t$NDZ_sanemsanas_datums[1:3]) != 0) && diff(t$NDZ_sanemsanas_datums[3:4]) == 0) {
        if (t$period[1] == "__________" && t$PS_code[1] == '__________' && t$NM_code[1] == '__________') {
          yt$dienas <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[2], prev, units = "days")) - 1,
                            as.numeric(diff(t$NDZ_sanemsanas_datums[3:4])))
        } else {stop("starpkodi4_50_50: Trūkst izstrādes koda.")}
      } else {stop("starpkodi4_50_50: Trūkst izstrādes koda.")}  
    } else {stop("starpkodi4_50_50: Trūkst izstrādes koda.")}
  } else {stop("starpkodi4_50_50: Trūkst izstrādes koda.")}    
  
  
  rm(y, t, prev, v)
  return(yt)
}

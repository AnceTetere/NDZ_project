starpkodi4_50_50_51 <- function(y, t, prev, v) {
  
  yt <- y[v,]
  
    if (t$zinkod[4] %in% c("21", "22", "23", "24", "25", "29")) {
                if (diff(t$NDZ_sanemsanas_datums[2:3]) == 0 && all(sapply(c(1,3), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
                  if (t$PS_code[1] == '_______' && t$NM_code[1] == '_______') {
                    yt$dd <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[2], prev, units = "days")) - 1, 
                                     as.numeric(diff(t$NDZ_sanemsanas_datums[3:4])) + 1) 
                  } else {stop("starpkodi4_50_50: Trūkst izstrādes koda.")}
                } else {stop("starpkodi4_50_50: Trūkst izstrādes koda.")}
    } else if (t$zinkod[4] %in% c("41", "51", "54", "92")) {
                if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
                  if (t$period[1] == "______" && t$PS_code[1] == '_______' && t$NM_code[1] == '_______') {
                    yt$dd <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1, 
                                     as.numeric(difftime(t$last_date[4], t$NDZ_sanemsanas_datums[4], units = "days")) + 1) 
                  } else if (t$period[1] == "_______" && t$PS_code[1] == '_______' && t$NM_code[1] == '_______') {
                    yt$dd <- as.numeric(difftime(t$last_date[4], t$NDZ_sanemsanas_datums[4], units = "days")) + 1
                  } else {stop("starpkodi4_50_50: Trūkst izstrādes koda.")}
                } else {stop("starpkodi4_50_50: Trūkst izstrādes koda.")}
    } else if (t$zinkod[4] %in% c("40", "50", "53", "91")) {
      if (diff(t$NDZ_sanemsanas_datums[1:2]) == 0 && all(diff(t$NDZ_sanemsanas_datums[2:4]) != 0)) {
        if(t$PS_code[1] == '_______' && t$NM_code[1] == '_______') {
          yt$dd <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[2], prev, units = "days")) - 1, 
                           as.numeric(diff(t$NDZ_sanemsanas_datums[3:4])))
        } else {stop("starpkodi4_50_50: Trūkst izstrādes koda.")}
      } else if (diff(t$NDZ_sanemsanas_datums[3:4]) == 0 && all(diff(t$NDZ_sanemsanas_datums[1:3]) != 0)) {
        if(t$period[1] == "______" && t$PS_code[1] == '_______' && t$NM_code[1] == '_______') {
          yt$dd <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[2], prev, units = "days")) - 1, 
                           as.numeric(diff(t$NDZ_sanemsanas_datums[3:4])))
          ZERO_plus(t[4,])
        } else {stop("starpkodi4_50_50: Trūkst izstrādes koda.")}
      } else {stop("starpkodi4_50_50: Trūkst izstrādes koda.")}
    } else {stop("starpkodi4_50_50: Trūkst izstrādes koda.")} 
  
  rm(y, t, prev, v)
  return(yt)
}

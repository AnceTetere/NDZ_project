starpkodi4_50_25 <- function(y, t, prev, v) {

  yt <- y[v, ] 
  
  if (t$zinkod[3] %in% c("41", "51", "54", "92")) {
          if (t$zinkod[4] %in% c("11", "14", "16", "61")) {
            if (all(sapply(c(1,3), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0)) && diff(t$NDZ_sanemsanas_datums[2:3]) == 0) {
              yt$dienas <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1, 
                               as.numeric(diff(t$NDZ_sanemsanas_datums[2:3])),
                               as.numeric(difftime(t$last_date[4], t$NDZ_sanemsanas_datums[4], units = "days")) + 1) 
            } else {stop("starpkodi4_50_25: Trūkst izstrādes koda.")}
          } else if (t$zinkod[4] %in% c("41", "51", "54", "92")) {
            if (diff(t$NDZ_sanemsanas_datums[2:3]) == 0 && all(sapply(c(1,3), function(i) all(diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0)))) {
              yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) 
            } else if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
              if(t$period[1] == "______" && t$PS_code[1] == "____________" && t$NM_code[1] == "____________") {
                yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) 
              } else {stop("starpkodi4_50_25: Trūkst izstrādes koda.")}
            } else {stop("starpkodi4_50_25: Trūkst izstrādes koda.")}
          } else if (t$zinkod[4] %in% c("21", "22", "23", "24", "25", "29")) {
            if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
              if (t$PS_code[1] == '____________' && t$NM_code[1] == '____________') {
                yt$dienas <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1,
                                 as.numeric(diff(t$NDZ_sanemsanas_datums[3:4])))
              } else {stop("starpkodi4_50_25: Trūkst izstrādes koda.")}
            } else {stop("starpkodi4_50_25: Trūkst izstrādes koda.")}
            } else if (t$zinkod[4] %in% c("40", "50", "53", "91")) {
              if (diff(t$NDZ_sanemsanas_datums[1:2]) != 0 && all(diff(t$NDZ_sanemsanas_datums[2:4]) == 0)) {
                if ((t$period[1] == '______' && t$PS_code[1] == '____________' && t$NM_code[1] == '____________') ||
                    (t$period[1] == '______' && t$PS_code[1] == '____________' && t$NM_code[1] == '____________')) {
                  yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1
                } else {stop("starpkodi4_50_25: Trūkst izstrādes koda.")}
              } else {stop("starpkodi4_50_25: Trūkst izstrādes koda.")}
            
          } else {stop("starpkodi4_50_25: Trūkst izstrādes koda.")}
  } else {stop("starpkodi4_50_25: Trūkst izstrādes koda.")}
    
  rm(y, t, prev, v)
  return(yt)
}

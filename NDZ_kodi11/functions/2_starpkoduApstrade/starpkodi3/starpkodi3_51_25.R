starpkodi3_51_25 <- function(y, t, prev, v) {
  
  yt <- y[v, ]
  
  #if (t$zinkod[3] == "50") {
  #        if (diff(t$NDZ_sanemsanas_datums[1:2]) != 0 && diff(t$NDZ_sanemsanas_datums[2:3]) == 0) {
  #          yt$dd <- as.numeric(diff(t$NDZ_sanemsanas_datums[1:2]) 
  #        } else {stop("Starpkodi3_51_25: Trūkst izstrādes koda.")}
  #@} else 
    if (t$zinkod[3] %in% c("11", "14", "16", "61")) {
          if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
                  yt$dd <- sum(as.numeric(diff(t$NDZ_sanemsanas_datums[1:2])) + 1,  
                                   as.numeric(difftime(t$last_date[3], t$NDZ_sanemsanas_datums[3], units = "days"))  + 1) 
          } else if (diff(t$NDZ_sanemsanas_datums[1:2]) != 0 && diff(t$NDZ_sanemsanas_datums[2:3]) == 0) {
                if (t$period[1] == "______" && t$PS_code[1] == "______" && t$NM_code[1] == "______") {
                  yt$dd <- sum(as.numeric(diff(t$NDZ_sanemsanas_datums[1:2])),
                                   as.numeric(difftime(t$last_date[3], t$NDZ_sanemsanas_datums[3], units = "days")))
                } else {stop("Starpkodi3_51_25: Trūkst izstrādes koda.")}
            } else {stop("Starpkodi3_51_25: Trūkst izstrādes koda.")}
    } else if (t$zinkod[3] %in% c("41", "51", "54", "92")) {
          if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
              if (t$PS_code[1] == '______' && t$NM_code[1] == '______') {
                yt$dd <- 0
              } else {
                yt$dd <- as.numeric(diff(t$NDZ_sanemsanas_datums[1:2])) + 1
              } 
          } else if (diff(t$NDZ_sanemsanas_datums[1:2]) != 0 && diff(t$NDZ_sanemsanas_datums[2:3]) == 0) {
              yt$dd <- as.numeric(diff(t$NDZ_sanemsanas_datums[2:3]))
      } else {stop("Starpkodi3_51_25: Trūkst izstrādes koda.")}
    } else if (t$zinkod[3] %in% c("40", "50", "53", "91")) {
             if (diff(t$NDZ_sanemsanas_datums[1:2]) != 0 && diff(t$NDZ_sanemsanas_datums[2:3]) == 0) {
                yt$dd <- as.numeric(diff(t$NDZ_sanemsanas_datums[1:2]))
      } else {stop("Starpkodi3_51_25: Trūkst izstrādes koda.")}
    } else {stop("Starpkodi3_51_25: Trūkst izstrādes koda.")}

  rm(y, t, prev, v)
  return(yt)
}

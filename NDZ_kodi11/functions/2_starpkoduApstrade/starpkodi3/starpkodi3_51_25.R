starpkodi3_51_25 <- function(y, t, prev, v) {
  
  #if (t$zinkod[3] == "50") {
  #        if (diff(t$NDZ_sanemsanas_datums[1:2]) != 0 && diff(t$NDZ_sanemsanas_datums[2:3]) == 0) {
  #          yt <- y[v, ]
  #          yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[1:2]) 
  #        } else {stop("Starpkodi3_51_25: Trūkst izstrādes koda.")}
  #} else (t$zinkod[3] == "11") {
  #        if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
  #          yt <- y[v, ]
  #          yt$dienas <- sum(as.numeric(diff(t$NDZ_sanemsanas_datums[1:2]) + 1,  
  #                              as.numeric(difftime(t$last_date[3], t$NDZ_sanemsanas_datums[3], units = "days"))  + 1) 
  #} else if (t$zinkod[2] == "21" && t$zinkod[3] == "11" && all(!diff(t$NDZ_sanemsanas_datums) == 0)) {
  #  days1 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[2], t$NDZ_sanemsanas_datums[1], units = "days")) + 1  
  #  days2 <- as.numeric(difftime(t$last_date[3], t$NDZ_sanemsanas_datums[3], units = "days"))  + 1 
  #  
  #  yt <- y2[v, ]
  #  yt$dienas <- sum(days1, days2)
  #  rm(days1, days2)
  
  #        } else {stop("Starpkodi3_51_25: Trūkst izstrādes koda.")}
  #} else 
    if (t$zinkod[3] %in% c("41", "51", "54", "92")) {
          if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
              if (t$PS_code[1] == '____________' && t$NM_code[1] == '____________') {
                yt <- y[v, ]
                yt$dienas <- 0
              } else {
                yt <- y[v, ]
                yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[1:2])) + 1
              } 
          } else if (diff(t$NDZ_sanemsanas_datums[1:2]) != 0 && diff(t$NDZ_sanemsanas_datums[2:3]) == 0) {
              yt <- y[v, ]
              yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[2:3]))
      } else {stop("Starpkodi3_51_25: Trūkst izstrādes koda.")}
  } else {stop("Starpkodi3_51_25: Trūkst izstrādes koda.")}

  rm(y, t, prev, v)
  return(yt)
}
  

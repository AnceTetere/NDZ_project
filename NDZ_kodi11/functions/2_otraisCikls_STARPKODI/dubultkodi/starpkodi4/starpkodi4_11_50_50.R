starpkodi4_11_50_50 <- function(y, t, prev, v) {
  
  yt <- y[v, ]
  
  if (t$zinkod[4] %in% c("41", "51", "54", "92")) {
            if (all(sapply(c(1,3), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0)) && diff(t$NDZ_sanemsanas_datums[2:3]) == 0) {
              yt$dienas <- as.numeric(sum(diff(t$NDZ_sanemsanas_datums[1:2]),
                                          difftime(t$last_date[4], t$NDZ_sanemsanas_datums[4], units = "days") + 1))
            } else if (all(diff(t$NDZ_sanemsanas_datums[1:3]) != 0) && diff(t$NDZ_sanemsanas_datums[3:4]) == 0) {
              if(t$period[1] == "_____" && t$PS_code[1] == "__________" && t$NM_code[1] == "__________") {
                t <- t[c(1,2,4,3),]
                rownames(t) <- NULL
                yt$dienas <- sum(sapply(c(1,3), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)])))
                
                ZERO_plus(t %>% slice(4))
              } else {stop("starpkodi4_11_50_50: Trūkst izstrādes koda.")}
            } else if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
              yt$dienas <- as.numeric(sum(diff(t$NDZ_sanemsanas_datums[c(1,3)]),
                                          difftime(t$last_date[4], t$NDZ_sanemsanas_datums[4], units = "days") + 1))
            } else {stop("starpkodi4_11_50_50: Trūkst izstrādes koda.")}
  } else if (t$zinkod[4] %in% c("21", "22", "23", "24", "25", "29")) {
          if (all(diff(t$NDZ_sanemsanas_datums[1:3]) == 0) && diff(t$NDZ_sanemsanas_datums[3:4]) != 0) {
              if (t$period[1] == "_____" && t$PS_code[1] == '__________' && t$NM_code[1] == '__________') {
                yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[1:2]))
              } else {stop("starpkodi4_11_50_50: Trūkst izstrādes koda.")}
          } else if (diff(t$NDZ_sanemsanas_datums[1:2]) == 0 && all(diff(t$NDZ_sanemsanas_datums[2:4]) != 0)) {
            if (t$period[1] == "_____" && t$PS_code[1] == '__________' && t$NM_code[1] == '__________') {
              yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[c(1,3)]))
            } else {stop("starpkodi4_11_50_50: Trūkst izstrādes koda.")}
          } else {stop("starpkodi4_11_50_50: Trūkst izstrādes koda.")}
  } else {stop("starpkodi4_11_50_50: Trūkst izstrādes koda.")}

  rm(y, t, prev, v)
  return(yt)
}


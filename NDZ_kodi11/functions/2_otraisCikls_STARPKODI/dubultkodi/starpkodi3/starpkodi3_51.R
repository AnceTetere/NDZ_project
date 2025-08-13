starpkodi3_51 <- function(y, t, prev, v) {
  
  yt <- y[v, ]

    if (t$zinkod[2] %in% c("40", "50", "53", "91")) {
       yt <- starpkodi3_51_50(y, t, prev, v)
  } else if (t$zinkod[2] %in% c("41", "51", "54", "92")) {
        if (t$zinkod[3] %in% c("21", "22", "23", "24", "25", "29")) {
            if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
              yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[2:3])) + 1 
            } else if (diff(t$NDZ_sanemsanas_datums[1:2]) == 0 && diff(t$NDZ_sanemsanas_datums[2:3]) != 0) {
              yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[2:3])) + 1 
              ZERO_minus(t %>% slice(1,2))
            } else {stop("Starpkodi3_51: Trūkst izstrādes koda.")}
        } else if (t$zinkod[3] %in% c("40", "50", "53", "91")) {
              yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[2:3]))
        } else if (t$zinkod[3] %in% c("41", "51", "54", "92")) {
          yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[2:3]))
        } else {stop("Starpkodi3_51: Trūkst izstrādes koda.")}
    } else if (t$zinkod[2] %in% c("21", "22", "23", "24", "25", "29")) {
        yt <- starpkodi3_51_25(y, t, prev, v)          
    } else if (t$zinkod[2] %in% c("11", "14", "16", "61")) {
        yt <- starpkodi3_51_11(y, t, prev, v)          
    } else {stop("Starpkodi3_51: Trūkst izstrādes koda.")}

    rm(y, t, prev, v)
    return(yt) 
  }

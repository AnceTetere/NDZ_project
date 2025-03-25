starpkodi8_50 <- function(y, t, prev, v) {
  
  yt <- y[v, ]

  if (t$zinkod[2] %in% c("41", "51", "54", "92")) {
    if (t$zinkod[3] %in% c("41", "51", "54", "92")) {
      if (t$zinkod[4] %in% c("40", "50", "53", "91")) {
        if (t$zinkod[5] %in% c("41", "51", "54", "92")) {
          if (t$zinkod[6] %in% c("40", "50", "53", "91")) {
            if (t$zinkod[7] %in% c("41", "51", "54", "92")) {
              if (t$zinkod[8] %in% c("41", "51", "54", "92")) {
                if (all(sapply(c(1,6), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
                    all(sapply(c(2,3,4,5,7), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
                  if (t$period[1] == "______" && t$PS_code[1] == "______" && t$NM_code[1] == "______"){
                    t <- t[c(2,1,3,4,7,6,8), ]; rownames(t) <- NULL
                    
                    yt$dienas <- sum(sapply(seq(1,6,by=2), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)])),
                                     as.numeric(difftime(t$last_date[8], t$NDZ_sanemsanas_datums[8], units = "days")))
                    ZERO_minus(t %>% slice(1))
                  } else {stop("starpkodi8_50 iztrūkst apstrādes koda.")}
                } else {stop("starpkodi8_50 iztrūkst apstrādes koda.")} 
              } else {stop("starpkodi8_50 iztrūkst apstrādes koda.")}
            } else {stop("starpkodi8_50 iztrūkst apstrādes koda.")}
          } else {stop("starpkodi8_50 iztrūkst apstrādes koda.")}
        } else {stop("starpkodi8_50 iztrūkst apstrādes koda.")}
      } else {stop("starpkodi8_50 iztrūkst apstrādes koda.")}
      } else if (t$zinkod[3] %in% c("40", "50", "53", "91")) {
        if (t$zinkod[4] %in% c("41", "51", "54", "92")) {
          if (t$zinkod[5] %in% c("40", "50", "53", "91")) {
            if (t$zinkod[6] %in% c("41", "51", "54", "92")) {
              if (t$zinkod[7] %in% c("40", "50", "53", "91")) {
                if (t$zinkod[8] %in% c("41", "51", "54", "92")) {
                  if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
                    
                    yt$dienas <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1,
                                     sapply(c(2,4,6), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)])),
                                     as.numeric(difftime(t$last_date[8], t$NDZ_sanemsanas_datums[8], units = "days")))
                  } else {stop("starpkodi8_50 iztrūkst apstrādes koda.")} 
                } else if (t$zinkod[8] %in% c("21", "22", "23", "24", "25", "29")) {
                  if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
                    yt$dienas <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1,
                                     sapply(c(2,4,6), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)])))
                  } else {stop("starpkodi8_50 iztrūkst apstrādes koda.")} 
                } else {stop("starpkodi8_50 iztrūkst apstrādes koda.")}
              } else {stop("starpkodi8_50 iztrūkst apstrādes koda.")}
            } else {stop("starpkodi8_50 iztrūkst apstrādes koda.")}
          } else {stop("starpkodi8_50 iztrūkst apstrādes koda.")}
        } else {stop("starpkodi8_50 iztrūkst apstrādes koda.")}
    } else {stop("starpkodi8_50 iztrūkst apstrādes koda.")}
  } else {stop("starpkodi8_50 iztrūkst apstrādes koda.")}  

  rm(y, t, prev, v)
  return(yt)
}

#} else if (all(t$zinkod[2:8] == c("51", "50", "51", "50", "51", "50", "25")) && 
#           all(diff(t$NDZ_sanemsanas_datums) != 0)) {
#  yt$dienas <- as.numeric(sum(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days") - 1, 
#                              diff(t$NDZ_sanemsanas_datums[2:3]), diff(t$NDZ_sanemsanas_datums[4:5]),
#                              diff(t$NDZ_sanemsanas_datums[6:7]))) 

#  
#} else if (all(t$zinkod[c(4,6)] == "50") && all(t$zinkod[c(2,3,5,7)] == "51") && t$zinkod[8] == "25" && 
#           all(diff(t$NDZ_sanemsanas_datums) != 0)) {
#  yt$dienas <- as.numeric(sum(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days") - 1, 
#                              diff(t$NDZ_sanemsanas_datums[3:4]), diff(t$NDZ_sanemsanas_datums[5:6]),
#                              diff(t$NDZ_sanemsanas_datums[7:8]) + 1)) 
#  
#} else if (all(t$zinkod[c(1,5,7)] == "50") && all(t$zinkod[c(2,6,8)] == "51") && t$zinkod[3] == "53" && t$zinkod[4] == "54" &&
#           all(diff(t$NDZ_sanemsanas_datums) != 0)) {
#  days <- as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1
#  for (d in seq(2,6,by=2)) {days <- days + as.numeric(difftime(t$NDZ_sanemsanas_datums[d+1], t$NDZ_sanemsanas_datums[d], units = "days"))}
#  days <- days + as.numeric(difftime(t$last_date[8], t$NDZ_sanemsanas_datums[8], units = "days")) + 1
#  
#  yt$dienas <- days
#  rm(days, d)


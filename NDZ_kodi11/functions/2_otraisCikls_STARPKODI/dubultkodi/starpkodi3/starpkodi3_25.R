starpkodi3_25 <- function(y, t, prev, v) {
  
  yt <- y[v,]

  if (t$zinkod[2] %in% c("11", "14", "16", "61")) {
    yt <- starpkodi3_25_11(y, t, prev, v)
  } else if (t$zinkod[2] %in% c("41", "51", "54", "92")) {
    yt <- starpkodi3_25_51(y, t, prev, v)
  } else if (t$zinkod[2] %in% c("21", "22", "23", "24", "25", "29")) {
          if (diff(t$NDZ_sanemsanas_datums[2:3]) == 0) {
            yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[2], prev, units = "days")) - 1
          } else {stop("Starpkodi3_25: Trūkst izstrādes koda.")}
  } else if (t$zinkod[2] %in% c("40", "50", "53", "91")) {
          if (t$zinkod[3] %in% c("41", "51", "54", "92")) {
            if (all(diff(t$NDZ_sanemsanas_datums) == 0)) {
              yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days"))
            } else if (diff(t$NDZ_sanemsanas_datums[2:3]) == 0 && diff(t$NDZ_sanemsanas_datums[1:2]) != 0) {
              yt <- y[v:(v+1), ]
              yt <- yt[yt$zinkod == "11", ]
            } else if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
              yt <- y[v:(v+1), ]
              yt <- yt[yt$zinkod == "11", ]
            } else if (diff(t$NDZ_sanemsanas_datums[2:3]) != 0 && diff(t$NDZ_sanemsanas_datums[1:2]) == 0) {
              yt <- y[v:(v+1), ]
              yt <- yt[yt$zinkod == "11", ]
            } else {stop("Starpkodi3_25: Trūkst izstrādes koda.")}
          } else {stop("Starpkodi3_25: Trūkst izstrādes koda.")}
  } else {stop("Starpkodi3_25: Trūkst izstrādes koda.")}

    rm(y, t, prev, v)
    return(yt) 
}    
    
#  
#    if (t$zinkod[2] == "41" && t$zinkod[3] == "11" && 
#             all(diff(t$NDZ_sanemsanas_datums[1:2]) == 0) && 
#             all(diff(t$NDZ_sanemsanas_datums[2:3]) != 0)) {
#    yt <- y[v, ]
#    yt$dienas <- as.numeric(difftime(t$last_date[3], t$sak_darbu[3], units = "days")) + 1 #jo darbs
#    } else } else if (t$zinkod[2] %in% c("92","41") && t$zinkod[3] %in% c("92","41") && all(diff(t$NDZ_sanemsanas_datums) == 0)) {
#      yt <- y[v, ]
#      yt$dienas <- 0
#    } else if (t$zinkod[2] == "41" && t$zinkod[3] == "11" && all(diff(t$NDZ_sanemsanas_datums) != 0)) {
#      yt <- y[v:(v+1), ]
#      yt <- yt[yt$zinkod == "11", ]
#    } else if (t$zinkod[2] == "41" && t$zinkod[3] == "41" && diff(t$NDZ_sanemsanas_datums[1:2]) == 0 &&
#               diff(t$NDZ_sanemsanas_datums[2:3]) != 0) {
#      yt <- y[v:(v+1), ]
#      yt <- yt[yt$zinkod == "11", ]
#    } else {stop("Starpkodi3_25: Trūkst izstrādes koda.")}
#  
#  
#  
#  
#    

#} else if (t$zinkod[2] == "92" && t$zinkod[3] == "41" && all(diff(t$NDZ_sanemsanas_datums) == 0)) {
#  yt <- y[v, ]
#  yt$dienas <- 0
#} else {
#  stop("Starpkodi3_21: Trūkst izstrādes koda.")
#}

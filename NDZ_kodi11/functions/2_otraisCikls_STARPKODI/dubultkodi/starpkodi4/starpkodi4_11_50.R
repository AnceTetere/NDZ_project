starpkodi4_11_50 <- function(y, t, prev, v) {
  
  if(t$zinkod[3] %in% c("41", "51", "54", "92")) {
    yt <- starpkodi4_11_50_51(y, t, prev, v)
  } else if (t$zinkod[3] %in% c("21", "22", "23", "24", "25", "29")) {
    yt <- starpkodi4_11_50_25(y, t, prev, v)
  } else if (t$zinkod[3] %in% c("11", "14", "16", "61")) {
    yt <- starpkodi4_11_50_11(y, t, prev, v)
  } else if (t$zinkod[3] %in% c("40", "50", "53", "91")) {
    yt <- starpkodi4_11_50_50(y, t, prev, v)
  } else {stop("Starpkodi4_11_50: Trūkst izstrādes koda.")}
  
  return(yt) 
}

#starpkodi4_11_53 <- function(y2, t, prev, v) {
#  
#  if (t$zinkod[3] == "54" && t$zinkod[4] == "53" && all(!diff(t$NDZ_sanemsanas_datums) == 0)) {
#    days1 <- as.numeric(difftime(t$beidz_darbu[2], t$sak_darbu[1], units = "days")) 
#    days2 <- as.numeric(difftime(t$beidz_darbu[4], t$sak_darbu[3], units = "days"))
#    
#    yt <- y2[v, ]
#    yt$dienas <- sum(days1, days2)
#    rm(days1, days2)
#  } else if (t$zinkod[3] == "25" && t$zinkod[4] == "54" && 
#             all(diff(t$NDZ_sanemsanas_datums[1:3]) != 0) && diff(t$NDZ_sanemsanas_datums[3:4]) == 0) {
#    yt <- y2[v, ]
#    yt$dienas <- as.numeric(difftime(t$beidz_darbu[2], t$sak_darbu[1], units = "days"))
#  } else {
#    stop("Starpkodi4_11_53: Trūkst izstrādes koda.")
#  }
#  
#  if(is.na(yt$PS_code[1])) {stop("Dienas NA.")}
#  return(yt) 
#}

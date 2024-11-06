starpkodi4_50_51_51 <- function(y, t, prev, v) {
  
  if (t$zinkod[4] %in% c("40", "50", "53", "91")) {
   # if (diff(t$NDZ_sanemsanas_datums[1:2]) == 0 && all(diff(t$NDZ_sanemsanas_datums[2:4]) != 0)) {
   #       yt <- y[v, ]
   #       yt$dienas <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1, 
  #                         as.numeric(diff(t$NDZ_sanemsanas_datums[3:4])))
   # } else 
      if (all(sapply(c(1,3), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) && diff(t$NDZ_sanemsanas_datums[2:3]) != 0) {
        if (t$PS_code[1] == "__________" && t$NM_code[1] == "__________") {
          t <- slice(t, c(2,1,3,4)) 
          yt <- y[v, ]
          yt$dienas <- sum(sapply(c(1,3), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)])))
        } else {stop("starpkodi4_50_51_51: Trūkst izstrādes koda.")}
    } else {stop("starpkodi4_50_51_51: Trūkst izstrādes koda.")}
  } else {stop("starpkodi4_50_51_51: Trūkst izstrādes koda.")}
  
  rm(y, t, prev, v)
  return(yt)
}



#} else if (t$zinkod[4] == "21" &&
#            all(sapply(c(1,3), function(i) all(diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) && 
#            diff(t$NDZ_sanemsanas_datums[2:3]) == 0) {
#   days1 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1 
#   days2 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[4], t$NDZ_sanemsanas_datums[3], units = "days")) + 1 
#   
#   yt <- y[v, ]
#   yt$dienas <- sum(days1, days2)
#   rm(days1, days2)
# } else if (t$zinkod[4] == "21" && diff(t$NDZ_sanemsanas_datums[1:2]) == 0) {
#   yt <- y[v, ]
#   yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[4], t$NDZ_sanemsanas_datums[3], units = "days")) + 1 
# } else if (t$zinkod[4] %in% c("21","25") && all(diff(t$NDZ_sanemsanas_datums) != 0)) {
#   days1 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1
#   days2 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[4], t$NDZ_sanemsanas_datums[3], units = "days")) + 1
#   
#   yt <- y[v, ]
#   yt$dienas <- sum(days1, days2)
#   rm(days1, days2)
#  } else {stop("starpkodi4_50_51: Trūkst izstrādes koda.")}

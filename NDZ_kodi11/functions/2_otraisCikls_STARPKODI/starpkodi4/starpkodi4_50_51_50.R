starpkodi4_50_51_50 <- function(y, t, prev, v) {
  
  if (t$zk[4] %in% c("21", "22", "23", "24", "25", "29")) {
    if (diff(t$NDZ_sanemsanas_datums[1:2]) == 0 && all(diff(t$NDZ_sanemsanas_datums[2:4]) != 0)) {
              if (t$PS_code[1] == "___________" && t$nmrkod[1] == "___________") {
                yt <- y[v,]
                yt$dd <- as.numeric(diff(t$NDZ_sanemsanas_datums[2:3]))
              } else {stop("starpkodi4_50_51: Trūkst izstrādes koda.")}
    } else if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
        yt <- y[v, ]
        yt$dd <- as.numeric(sum(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days") - 1, 
                                    diff(t$NDZ_sanemsanas_datums[2:3])))
   #} else if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
      #   days1 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1  
      #   days2 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[3], t$NDZ_sanemsanas_datums[2], units = "days"))
      #   
      #   yt <- y[v, ]
      #   yt$dd <- sum(days1, days2)
      #   rm(days1, days2)
    } else {stop("starpkodi4_50_51: Trūkst izstrādes koda.")}
    #} else if (t$zk[4] == "21") {
    #if (all(diff(t$NDZ_sanemsanas_datums[2:3]) == 0) && all(diff(t$NDZ_sanemsanas_datums[1:2]) != 0)&& all(diff(t$NDZ_sanemsanas_datums[3:4]) != 0)) {}
    #  yt <- y[v, ]
    #  yt$dd <- as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1 
    #    # } else {stop("starpkodi4_50_51: Trūkst izstrādes koda.")}
  } else {stop("starpkodi4_50_51: Trūkst izstrādes koda.")}
  
  rm(y, t, prev, v)
  return(yt)
}

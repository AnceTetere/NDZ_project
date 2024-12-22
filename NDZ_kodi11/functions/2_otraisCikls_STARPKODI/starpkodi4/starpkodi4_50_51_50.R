starpkodi4_50_51_50 <- function(y, t, prev, v) {
  
  if (t$zk[4] %in% c("21", "22", "23", "24", "25", "29")) {
             if (diff(t$NDZ_sanemsanas_datums[1:2]) == 0 && all(diff(t$NDZ_sanemsanas_datums[2:4]) != 0)) {
                       if (t$PS_code[1] == "_________" && t$NM_code[1] == "_________") {
                         yt <- y[v,]
                         yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[2:3]))
                       } else {stop("starpkodi4_50_51: Trūkst izstrādes koda.")}
             } else if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
                 yt <- y[v, ]
                 yt$dienas <- as.numeric(sum(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days") - 1, 
                                             diff(t$NDZ_sanemsanas_datums[2:3])))
             } else if (diff(t$NDZ_sanemsanas_datums[2:3]) == 0 && all(sapply(c(1,3), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)])))) {
                 yt <- y[v, ]
                 yt$dienas <- as.numeric(sum(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days") - 1, 
                                             diff(t$NDZ_sanemsanas_datums[2:3])))
             } else {stop("starpkodi4_50_51: Trūkst izstrādes koda.")}
  } else if (t$zk[4] %in% c("41", "51", "54", "92")) {
    if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
      yt <- y[v, ]
      yt$dienas <- as.numeric(sum(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days") - 1,
                                  diff(t$NDZ_sanemsanas_datums[2:3]),
                                  difftime(t$last_date[4], t$NDZ_sanemsanas_datums[4], units = "days") + 1))
    } else {stop("starpkodi4_50_51: Trūkst izstrādes koda.")}
  } else {stop("starpkodi4_50_51: Trūkst izstrādes koda.")}
  
  rm(y, t, prev, v)
  return(yt)
}

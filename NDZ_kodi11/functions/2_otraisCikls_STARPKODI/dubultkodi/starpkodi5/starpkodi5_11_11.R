starpkodi5_11_11 <- function(y, t, prev, v) {
  
  yt <- y[v, ]
  
  if (all(t$zinkod[3:5] == c("50", "51", "51"))) {
    if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
      yt$dienas <- as.numeric(sum(diff(t$NDZ_sanemsanas_datums[2:3]),
                                  difftime(t$last_date[5], t$NDZ_sanemsanas_datums[5], units = "days") + 1))
    } else {stop("Starpkodi5_11: Trūkst izstrādes koda.")}
  } else if (all(t$zinkod[3:5] == c("50", "25", "51"))) {
        if (all(diff(t$NDZ_sanemsanas_datums[1:4]) != 0) && diff(t$NDZ_sanemsanas_datums[4:5]) == 0) {
          yt$dienas <- sum(sapply(c(2,4), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)])))
        } else {stop("Starpkodi5_11: Trūkst izstrādes koda.")}
  } else {stop("Starpkodi5_11_11: Trūkst izstrādes koda.")}
  
  rm(y, t, prev, v)
  return(yt)
}

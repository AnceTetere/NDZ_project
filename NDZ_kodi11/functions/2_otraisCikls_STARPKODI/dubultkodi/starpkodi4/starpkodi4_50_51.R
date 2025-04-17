starpkodi4_50_51 <- function(y, t, prev, v) {
  
  if (t$zinkod[3] %in% c("40", "50", "53", "91")) {
    yt <- starpkodi4_50_51_50(y, t, prev, v)
  } else if (t$zinkod[3] %in% c("21", "22","23", "24", "25", "29")) {
    yt <- starpkodi4_50_51_25(y, t, prev, v)
  } else if (t$zinkod[3] %in% c("41", "51", "54", "92")) {
    yt <- starpkodi4_50_51_51(y, t, prev, v)
  } else {stop("starpkodi4_50_51: Trūkst izstrādes koda.")}

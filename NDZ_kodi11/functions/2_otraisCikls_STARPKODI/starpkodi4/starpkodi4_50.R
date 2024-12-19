starpkodi4_50 <- function(y, t, prev, v) {

  if (t$zk[2] %in% c("40", "50", "53", "91")) {
    yt <- starpkodi4_50_50(y, t, prev, v)
  } else if (t$zk[2] %in% c("41", "51", "54", "92")) {
    yt <- starpkodi4_50_51(y, t, prev, v)
  } else if (t$zk[2] %in% c("21", "22", "23", "24", "25", "29")) {
    yt <- starpkodi4_50_25(y, t, prev, v)
  } else {stop("starpkodi4_50: Trūkst izstrādes koda.")}
  
  rm(y, t, prev, v)  
  return(yt) 
}

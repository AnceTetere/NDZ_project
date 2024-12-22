starpkodi3_11 <- function(y, t, prev, v) {
  
  if (t$zk[2] %in% c("11", "14", "16", "61")) {
    yt <- starpkodi3_11_16(y, t, prev, v)
  } else if (t$zk[2] %in% c("21", "22", "23", "24", "25", "29")) {
    yt <- starpkodi3_11_29(y, t, prev, v)
  } else if (t$zk[2] %in% c("40", "50", "53", "91")) {
    yt <- starpkodi3_11_50(y, t, prev, v)
  } else if (t$zk[2]  %in% c("41", "51", "54", "92")) {
    yt <- starpkodi3_11_51(y, t, prev, v)  
  } else {stop("Starpkodi3_11: Trūkst izstrādes koda.")}
  
  rm(y, t, prev, v)
  return(yt) 
}

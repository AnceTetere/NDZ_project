starpkodi7_51 <- function(y, t, prev, v) {
  
  if (t$zk[2] %in% c("40", "50", "53", "91")) {
    yt <- starpkodi7_51_50(y, t, prev, v)
  } else if (t$zk[2] %in% c("11", "14", "16", "61")) {
    yt <- starpkodi7_51_11(y, t, prev, v)
  } else {stop("Starpkodi7_51 iztrūkst apstrādes koda.")}
  
  rm(y, t, prev, v)
  return(yt)
}

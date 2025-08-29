starpkodi7_50 <- function(y, t, prev, v) {
  
  if (t$zinkod[2] %in% c("40", "50", "53", "91")) {
    stop("Starpkodi7_51 iztrūkst apstrādes koda.")
  } else if (t$zinkod[2] %in% c("11", "14", "16", "61")) {
    stop("Starpkodi7_51 iztrūkst apstrādes koda.")
  } else if (t$zinkod[2] %in% c("41", "51", "54", "92")) {
    yt <- starpkodi7_50_51(y, t, prev, v)
  } else {stop("Starpkodi7_51 iztrūkst apstrādes koda.")}
  
  rm(y, t, prev, v)
  return(yt)
}
 

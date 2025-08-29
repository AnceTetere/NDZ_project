starpkodi7 <- function(y, t, prev, v) {
  
  if (t$zinkod[1] %in% c("41", "51", "54", "92")) {
    yt <- starpkodi7_51(y, t, prev, v)
  } else if (t$zinkod[1] %in% c("11", "14", "16", "61")) {
    yt <- starpkodi7_11(y, t, prev, v)
  } else if (t$zinkod[1] %in% c("40", "50", "53", "91")) {
    yt <- starpkodi7_50(y, t, prev, v)
  } else {stop("Starpkodi4 iztrūkst apstrādes koda.")}
  
  if(is.na(yt$PS_code[1])) {stop("Dienas NA.")}
  yt$zinkod <- "combined"  #jo starpkodu dienu sarēķins
  
  rm(y, t, prev, v)
  return(yt)
}

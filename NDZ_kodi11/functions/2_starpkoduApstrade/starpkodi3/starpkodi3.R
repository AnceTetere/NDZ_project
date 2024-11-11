starpkodi3 <- function(y, t, prev, v) {
  
  if (t$zinkod[1] == "11") {
    yt <- starpkodi3_11(y, t, prev, v)
  } else if (t$zinkod[1] %in% c("41", "51", "54", "92")) {
    yt <- starpkodi3_51(y, t, prev, v)
  } else if (t$zinkod[1] %in% c("40", "50", "53", "91")) {
     yt <- starpkodi3_50(y, t, prev, v)
  } else if (t$zinkod[1] %in% c("21", "22", "23", "24", "25", "29")) {
    yt <- starpkodi3_25(y, t, prev, v)

  } else {stop("Starpkodi3: Trūkst izstrādes koda.")}
  
  if(is.na(yt$pseidokods[1])) {stop("Dienas NA.")}
  yt$zinkod <- "combined"  #jo starpkodu dienu sarēķins
  
  return(yt)
}

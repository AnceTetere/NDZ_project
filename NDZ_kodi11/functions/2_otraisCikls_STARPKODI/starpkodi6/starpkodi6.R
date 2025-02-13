starpkodi6 <- function(y, t, prev, v) {
  
  if (t$zinkod[1] %in% c("11", "14", "16", "61")) {
    yt <- starpkodi6_11(y, t, prev, v)
  } else if (t$zinkod[1] %in% c("21", "22", "23", "24", "25", "29")) {
    yt <- starpkodi6_25(y, t, prev, v)
  } else if (t$zinkod[1] %in% c("41", "51", "54", "92")) {
    yt <- starpkodi6_51(y, t, prev, v)
 # } else if (t$zinkod[1] == "50") {
 #   yt <- starpkodi6_50(y, t, prev, v)
 # } else if (t$zinkod[1] == "26") {
 #   yt <- starpkodi6_26(y, t, prev, v)
 # } else 
 } else if (t$zinkod[1] %in% c("40", "50", "53", "91")) {
    yt <- starpkodi6_91(y, t, prev, v)
  } else {stop("Starpkodi6 iztrūkst apstrādes koda.")}
  
  if(is.na(yt$PS_code[1])) {stop("DD NA.")}
  yt$zinkod <- "combined"  
  return(yt)
}

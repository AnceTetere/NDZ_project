dubultkodi <- function(y_2plus, t, prev, v, z) {
  
  if (nrow(t) == 2) {
    z <- rbind(z, starpkodi2(y_2plus, t, prev, v))
  } else if(nrow(t) == 3){
    z <- rbind(z, starpkodi3(y_2plus, t, prev, v))  
  } else if (nrow(t) == 4) {
    z <- rbind(z, starpkodi4(y_2plus, t, prev, v))
  } else if (nrow(t) == 5) {
    z <- rbind(z, starpkodi5(y_2plus, t, prev, v))
  } else if (nrow(t) == 6) {
    z <- rbind(z, starpkodi6(y_2plus, t, prev, v))
  } else if (nrow(t) == 7) {
    z <- rbind(z, starpkodi7(y_2plus, t, prev, v))
  } else if (nrow(t) == 8) {
    z <- rbind(z, starpkodi8(y_2plus, t, prev, v))
  } else if (nrow(t) == 9) {
    z <- rbind(z, starpkodi9(y_2plus, t, prev, v))
  } else if (nrow(t) == 10) {
    z <- rbind(z, starpkodi10(y_2plus, t, prev, v))
  } else if (nrow(t) == 13) {
    z <- rbind(z, starpkodi13(y_2plus, t, prev, v))
  } else {
    stop("Trūkst izstrādes koda.")
  }
  
  return(z)
}

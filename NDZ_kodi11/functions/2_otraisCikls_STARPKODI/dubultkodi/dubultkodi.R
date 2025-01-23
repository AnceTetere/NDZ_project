#dubultkodi <- function(y_2plus, t, prev, v, z) {
dubultkodi <- function(y, t, prev, v, z) {
#for tests y <- y_2plus
  
  if (nrow(t) == 2) {
    z <- rbind(z, starpkodi2(y, t, prev, v))
    #FOR TESTS z <- rbind(z, yt)
  } else if(nrow(t) == 3){
    z <- rbind(z, starpkodi3(y, t, prev, v))
  } else if (nrow(t) == 4) {
    z <- rbind(z, starpkodi4(y, t, prev, v))
  } else if (nrow(t) == 5) {
    z <- rbind(z, starpkodi5(y, t, prev, v))
  } else if (nrow(t) == 6) {
    z <- rbind(z, starpkodi6(y, t, prev, v))
  } else if (nrow(t) == 7) {
    z <- rbind(z, starpkodi7(y, t, prev, v))
  } else if (nrow(t) == 8) {
    z <- rbind(z, starpkodi8(y, t, prev, v))
  } else if (nrow(t) == 9) {
    z <- rbind(z, starpkodi9(y, t, prev, v))
  } else if (nrow(t) == 10) {
    z <- rbind(z, starpkodi10(y, t, prev, v))
  } else if (nrow(t) == 11) {
    z <- rbind(z, starpkodi11(y, t, prev, v))
  #} else if (nrow(t) == 13) {
  #  z <- rbind(z, starpkodi13(y_2plus, t, prev, v))
  #} else if (nrow(t) == 16) {
  #  z <- rbind(z, starpkodi16(y_2plus, t, prev, v))
  } else if (nrow(t) == 17) {
    z <- rbind(z, starpkodi17(y, t, prev, v))
  } else {stop("dubultkodi: Trūkst izstrādes koda.")}
  
  return(z)
}

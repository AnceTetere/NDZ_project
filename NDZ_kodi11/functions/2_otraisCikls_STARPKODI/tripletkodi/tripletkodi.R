tripletkodi <- function(y, t, prev, v, z) {
  #y <- y_2plus
  
  if(nrow(t) == 3){
    z <- rbind(z, tripletkodi3(y, t, prev, v))
  } else if (nrow(t) == 4) {
    z <- rbind(z, tripletkodi4(y, t, prev, v))
  } else if (nrow(t) == 5) {
    z <- rbind(z, tripletkodi5(y, t, prev, v))
  } else if (nrow(t) == 6) {
    z <- rbind(z, tripletkodi6(y, t, prev, v))
  } else if(nrow(t) == 8){
    z <- rbind(z, tripletkodi8(y, t, prev, v))
  } else {stop("Tripletkodi trūkst izstrādes koda.")}
  
  return(z)
}

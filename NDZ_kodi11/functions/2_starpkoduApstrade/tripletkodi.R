tripletkodi <- function(y_2plus, t, prev, v, z) {
  
  if(nrow(t) == 3){
    z <- rbind(z, tripletkodi3(y_2plus, t, prev, v))
  } else if (nrow(t) == 4) {
    z <- rbind(z, tripletkodi4(y_2plus, t, prev, v))
  } else if (nrow(t) == 5) {
    z <- rbind(z, tripletkodi5(y_2plus, t, prev, v))
  } else {
    stop("Tripletkodi() trūkst izstrādes koda.")
  }
  
  return(z)
}

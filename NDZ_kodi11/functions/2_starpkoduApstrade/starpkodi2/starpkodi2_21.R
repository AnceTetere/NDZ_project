starpkodi2_21 <- function(y, t, prev, v) {
  
  if (t$zinkod[2] == "51") {
      yt <- y[v,] 
      yt$dienas <- 0
  } else {stop("Starpkodi2_21: Trūkst izstrādes koda.")}
  #} else if(t$zinkod[2] == "54") {
    #  yt <- y[v,] 
    #  yt$dienas <- 0
  #} else if (t$zinkod[2] == "41") {
    #  yt <- y2[v,] 
    #  yt$dienas <- 0

rm(y, t, prev, v)  
return(yt)
} 

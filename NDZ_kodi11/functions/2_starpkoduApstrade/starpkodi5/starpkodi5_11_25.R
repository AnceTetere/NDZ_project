starpkodi5_11_25 <- function(y, t, prev, v){
  
  if(t$zinkod[3] == '11') {
    if(t$zinkod[4] == '11') {
      if(t$zinkod[5] == '50') {
        if(diff(t$ZDN_sanemsanas_datums[1:2]) == 0 &&all(sapply(2:4, function(i) diff(t$ZDN_sanemsanas_datums[i:(i+1)]) != 0))){
          #Neesmu pārliecināta, ka šo var vispārināt, jo tikpat labi, indivīds var tikt atlaists un 
          #tad nevis ar diviem, bet trijiem kodiem '11' uzsāk darbu (kā vienmēr, aprēķiniem lieto vēlāko).
          if(t$pseidokods[1] == 'PK29AE498F3' && t$nmrkod[1] == '42103083297') {
            #Indivīds tiek pieņemts darbā, tad atlaists.
            #Tad ar diviem kodiem pieņemts darbā no jauna (lieto vēlāko) un aiziet bezalgas atvaļinājumā.
            yt <- y[v,]
            yt$dienas <- sum(as.numeric(diff(t$ZDN_sanemsanas_datums[1:2])), as.numeric(diff(t$ZDN_sanemsanas_datums[4:5])))
          } else {stop("Starpkodi5_11_25: Trūkst izstrādes koda.")}
        } else {stop("Starpkodi5_11_25: Trūkst izstrādes koda.")}
      } else {stop("Starpkodi5_11_25: Trūkst izstrādes koda.")}
    } else {stop("Starpkodi5_11_25: Trūkst izstrādes koda.")}
  } else {stop("Starpkodi5_11_25: Trūkst izstrādes koda.")}


  
  
  return(yt)
}

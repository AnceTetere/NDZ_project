starpkodi2_40 <- function(y, t, prev, v) {

  if (t$zinkod[2] == "50") {
         if (diff(t$NDZ_sanemsanas_datums[1:2]) == 0 &&
             (t$PS_code[1] == '_________' && t$NM_code[1] == '_________')) {
             yt <- y[v,]
           yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1 
         } else {stop("Starpkodi2_40: Trūkst izstrādes koda.")}
  } else {stop("Starpkodi2_40: Trūkst izstrādes koda.")}
  
  rm(y, t, prev, v)
  return(yt)
}

  
#           t$PS_code[1] == '_________' && t$NM_code[1] == '_________') {
#  yt <- y2[v,] 
#  yt$dienas <- as.numeric(difftime(t$beidz[1], prev, units = "days")) - 1 
#if (t$zinkod[2] == "25") {
#  yt <- y2[v:(v+1),] 
#  yt <- yt[yt$zinkod == "40", ]
#} else 

#} else if (t$zinkod[1] == "40" && t$zinkod[2] == "23") {
#  yt <- y2[v,] 
#  yt$dienas <- as.numeric(difftime(t$beidz[1], prev, units = "days")) - 1 

#} else if (t$zinkod[1] == "40" && t$zinkod[2] == "51" && t$PS_code[1] == '_________' && t$NM_code[1] == '_________') {
#  days1 <- as.numeric(difftime(t$beidz[1], prev, units = "days")) -1 
#  days2 <- as.numeric(difftime(t$last_date[2], t$sak[2], units = "days")) + 1 

#  yt <- y2[v,] 
#  yt$dienas <- sum(days1, days2)
#  rm(days1,days2)

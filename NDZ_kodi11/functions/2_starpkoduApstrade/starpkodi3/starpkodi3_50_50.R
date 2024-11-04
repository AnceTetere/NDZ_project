starpkodi3_50_50 <- function(y, t, prev, v) {
  
  if (t$zinkod[3] %in% c("21", "25")) {
           if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
             yt <- y[v:(v+1), ]
             yt <- yt[yt$zinkod == "50", ]
           } else {stop("starpkodi3_50_50: Iztrūkst apstrādes koda.")} 
  #} else if (t$zinkod[3] == "21") {  
          # if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
          #   yt <- y[v, ]
          #   yt$dienas <- as.numeric(difftime(t$sak_beidz[2], prev, units = "days")) - 1 
          # } else if (diff(t$NDZ_sanemsanas_datums[2:3]) != 0 && diff(t$NDZ_sanemsanas_datums[1:2]) == 0) {
          #   yt <- y[v, ]
          #   yt$dienas <- as.numeric(difftime(t$sak_beidz[2], prev, units = "days")) - 1   
          # } else {stop("starpkodi3_50: Iztrūkst apstrādes koda.")} 
  } else {stop("starpkodi3_50_50: Iztrūkst apstrādes koda.")} 
  
  rm(y, t, prev, v)
  return(yt)
}

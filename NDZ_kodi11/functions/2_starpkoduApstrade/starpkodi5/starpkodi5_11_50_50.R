starpkodi5_11_50_50 <- function(y, t, prev, v) {
  
  if (all(t$zinkod[4:5] %in% c("41", "51", "54", "92"))) {
         if (all(sapply(c(1,2,4),function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0)) && 
             diff(t$NDZ_sanemsanas_datums[3:4]) == 0) {
           yt <- y[v,]
           yt$dienas <- sum(as.numeric(diff(t$NDZ_sanemsanas_datums[1:2])),    
                            as.numeric(difftime(t$last_date[5], t$NDZ_sanemsanas_datums[5], units = "days")) + 1) 
         } else {stop("Starpkodi5_11_50_50: Trūkst izstrādes koda.")}
  } else {stop("Starpkodi5_11_50_50: Trūkst izstrādes koda.")}

  rm(y, t, prev, v)
  return(yt)
}

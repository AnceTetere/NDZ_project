starpkodi5_11_50_50 <- function(y, t, prev, v) {
  
  yt <- y[v,] 
  
  if (all(t$zinkod[4:5] %in% c("41", "51", "54", "92"))) {
         if (all(sapply(c(1,2,4),function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0)) && 
             diff(t$NDZ_sanemsanas_datums[3:4]) == 0) {
           yt$dienas <- sum(as.numeric(diff(t$NDZ_sanemsanas_datums[1:2])),    
                            as.numeric(difftime(t$last_date[5], t$NDZ_sanemsanas_datums[5], units = "days")) + 1) 
         } else if (all(sapply(c(2,4),function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) && 
                    all(sapply(c(1,3),function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
                   if ((t$period[1] == "_____" && t$PS_code[1] == "__________" && t$NM_code[1] == "__________") ||
                       (t$period[1] == "_____" && t$PS_code[1] == "__________" && t$NM_code[1] == "__________")) {
                     yt$dienas <- sum(as.numeric(diff(t$NDZ_sanemsanas_datums[1:2])),    
                                      as.numeric(difftime(t$last_date[5], t$NDZ_sanemsanas_datums[5], units = "days")) + 1)  
                   } else {stop("Starpkodi5_11_50_50: Trūkst izstrādes koda.")}
         } else if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
           if (t$period[1] == "_____" && t$PS_code[1] == "__________" && t$NM_code[1] == "__________") {
             yt$dienas <- sum(as.numeric(diff(t$NDZ_sanemsanas_datums[c(1,3)])),    
                              as.numeric(difftime(t$last_date[5], t$NDZ_sanemsanas_datums[5], units = "days")) + 1)  
           } else {stop("Starpkodi5_11_50_50: Trūkst izstrādes koda.")}
         } else {stop("Starpkodi5_11_50_50: Trūkst izstrādes koda.")}
  } else {stop("Starpkodi5_11_50_50: Trūkst izstrādes koda.")}

  rm(y, t, prev, v)
  return(yt)
}

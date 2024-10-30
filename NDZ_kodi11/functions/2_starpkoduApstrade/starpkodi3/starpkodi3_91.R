starpkodi3_91 <- function(y, t, prev, v) {
  
  if (t$zinkod[2] == "92") {
         yt <- starpkodi3_91_92(y, t, prev, v)
  } else if (t$zinkod[2] %in% c("21", "22", "23", "24", "25", "29")) {
         if(t$zinkod[3] == "92") {
           if(diff(t$NDZ_sanemsanas_datums[1:2]) != 0 && diff(t$NDZ_sanemsanas_datums[2:3]) == 0) {
             yt <- y[v, ]
             yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1 
           } else {stop("Starpkodi3_91: Trūkst izstrādes koda.")}
         } else {stop("Starpkodi3_91: Trūkst izstrādes koda.")}
  } else {stop("Starpkodi3_91: Trūkst izstrādes koda.")}


      
    
  #} else if (t$zinkod[2] == "51" && t$zinkod[3] == "92" && diff(t$NDZ_sanemsanas_datums[2:3]) != 0 && diff(t$NDZ_sanemsanas_datums[1:2]) == 0) {
  #  yt <- y[v, ]
  #  yt$dienas <- as.numeric(difftime(t$last_date[3], t$NDZ_sanemsanas_datums[3], units = "days")) + 1 
  
  rm(y, t, prev, v)
  return(yt) 
}

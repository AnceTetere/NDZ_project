starpkodi3_50 <- function(y, t, prev, v) {
  
  if(t$zinkod[2] == "51") {
       yt <- starpkodi3_50_51(y, t, prev, v)
  } else if (t$zinkod[2] == "21") {
       if(t$zinkod[3] == "51") {
         if(all(diff(t$NDZ_sanemsanas_datums) != 0)) {
           yt <- y[v, ]
           yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1 #jo atvaļinājums
         } else {stop("starpkodi3_50: Iztrūkst apstrādes koda.")}
       } else {stop("starpkodi3_50: Iztrūkst apstrādes koda.")} 
  } else if (t$zinkod[2] == "50") {
    yt <- starpkodi3_50_50(y, t, prev, v)
  } else if (t$zinkod[2] == "91") {
    yt <- starpkodi3_50_91(y, t, prev,v)
  } else {stop("starpkodi3_50: Iztrūkst apstrādes koda.")}  
  

  

    
  #} else if(t$zinkod[2] == "11") {
  #  yt <- starpkodi3_50_11(y, t, prev, v)
  #} else if (t$zinkod[2] %in% c("21","24") && t$zinkod[3] == "51" && 
  #           t$NDZ_sanemsanas_datums[2] == t$NDZ_sanemsanas_datums[3]){
  #  yt <- y[v, ]
  #  yt$dienas <- as.numeric(difftime(t$sak_beidz[1], prev, units = "days"))
  #} else if (t$zinkod[2] == "25" && t$zinkod[3] == "51" && t$NDZ_sanemsanas_datums[2] == t$NDZ_sanemsanas_datums[3]) {
  #  days <- as.numeric(difftime(t$sak_beidz[1], prev, units = "days")) - 1
    
   # yt <- y[v, ]
  #  yt$dienas <- days
  
  #} else if (t$zinkod[2] == "22" && t$zinkod[3] == "51" && all(diff(t$NDZ_sanemsanas_datums[1:2]) != 0) && all(diff(t$NDZ_sanemsanas_datums[2:3]) == 0)) {
  #  yt <- y[v, ]
  #  yt$dienas <- as.numeric(difftime(t$sak_beidz[1], prev, units = "days")) - 1
  #} else if (t$zinkod[2] == "25" && t$zinkod[3] == "51" && all(diff(t$NDZ_sanemsanas_datums) != 0)) {
  #  yt <- y[v, ]
  #  yt$dienas <- as.numeric(difftime(t$sak_beidz[1], prev, units = "days")) - 1
  #} else if (t$zinkod[2] == "40" && t$zinkod[3] == "51" && diff(t$NDZ_sanemsanas_datums[1:2]) != 0 && diff(t$NDZ_sanemsanas_datums[2:3]) == 0) {
  #  yt <- y[v, ]
  #  yt$dienas <- as.numeric(difftime(t$sak_beidz[1], prev, units = "days")) - 1
  #} else if (t$zinkod[2] == "53" && t$zinkod[3] == "54" && 
  #           diff(t$NDZ_sanemsanas_datums[1:2]) == 0 && diff(t$NDZ_sanemsanas_datums[2:3]) != 0 &&
  #           t$PS_code[1] == '____________' && t$NM_code[1] == '____________') {
  #  days1 <- as.numeric(difftime(t$sak_beidz[2], prev, units = "days")) - 1 
  #  days2 <- as.numeric(difftime(t$last_date[3], t$sak_beidz[3], units = "days")) + 1 
    
   # yt <- y[v, ]
  #  yt$dienas <- sum(days1, days2)
  #  rm(days1, days2)
  #} else if (t$zinkod[2] == "53" && t$zinkod[3] == "54" && 
  #           diff(t$NDZ_sanemsanas_datums[1:2]) == 0 && diff(t$NDZ_sanemsanas_datums[2:3]) != 0 &&
  #           t$PS_code[1] == '____________' && t$NM_code[1] == '____________') {
  #  days1 <- as.numeric(difftime(t$sak_beidz[2], prev, units = "days")) - 1 
  #  days2 <- as.numeric(difftime(t$last_date[3], t$sak_beidz[3], units = "days")) + 1 
    
  #  yt <- y[v, ]
  #  yt$dienas <- sum(days1, days2)
  #  rm(days1, days2)

  rm(y, t, prev, v)
  return(yt) 
}

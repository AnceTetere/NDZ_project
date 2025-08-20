starpkodi3_50_91 <- function(y, t, prev,v) {
  
  if (t$zinkod[3] %in% c("41", "51", "54", "92")) {
          if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
            if (t$PS_code[1] == '_________' && t$NM_code[1] == '_________') {
              t$zinkod[1] <- '92'; t$sak_beidz[1] <- '1'
              yt <- y[v,]
              yt$dienas <- sum(as.numeric(diff(t$NDZ_sanemsanas_datums[1:2])),
                               as.numeric(difftime(t$last_date[3], t$NDZ_sanemsanas_datums[3], units = "days")) + 1)
            } else if (t$PS_code[1] == '_________' && t$NM_code[1] == '_________') {
                  yt <- y[v,]
                  yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1
            } else {stop("starpkodi3_50_91: Iztrūkst apstrādes koda.")}
          } else {stop("starpkodi3_50_91: Iztrūkst apstrādes koda.")}    
  #} else if (t$zinkod[3] == "51") {
        #  if (diff(t$NDZ_sanemsanas_datums[1:2]) != 0 && diff(t$NDZ_sanemsanas_datums[2:3]) == 0) {
        #    yt <- y[v, ]
        #    yt$dienas <- as.numeric(difftime(t$sak_beidz[1], prev, units = "days")) - 1 
        #  } else {stop("starpkodi3_50: Iztrūkst apstrādes koda.")}
  } else {stop("starpkodi3_50: Iztrūkst apstrādes koda.")}
  
  rm(y, t, prev,v)
  return(yt)
}

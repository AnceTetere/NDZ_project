starpkodi4_25 <- function(y, t, prev, v) {
  
  if (t$zinkod[2] == "51") {
           if (t$zinkod[3] == "11") {
             if (t$zinkod[4] == "25") {
               if (diff(t$NDZ_sanemsanas_datums[1:2]) == 0 && diff(t$NDZ_sanemsanas_datums[3:4]) != 0) {
                 yt <- y[v, ]
                 yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[3:4]))
               } else {stop("1starpkodi4_25: Iztrūkst aptrādes koda.")}
             } else {stop("2starpkodi4_25: Iztrūkst aptrādes koda.")}
           } else if (t$zinkod[3] == "51") {
               if (t$zinkod[4] == "51") {
                if (all(diff(t$NDZ_sanemsanas_datums[2:4]) != 0) && diff(t$NDZ_sanemsanas_datums[1:2]) == 0) {
                   yt <- y[v, ]
                   yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days"))
                } else {stop("7starpkodi4_25: Iztrūkst aptrādes koda.")}
             } else {stop("8starpkodi4_25: Iztrūkst aptrādes koda.")}
           } else {stop("9starpkodi4_25: Iztrūkst aptrādes koda.")}
    
  } else if (t$zinkod[2] == "11") {
    if (t$zinkod[3] == "50") {
      if (t$zinkod[4] == "51") {
        if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
          yt <- y[v, ]
          yt$dienas <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")),
                           as.numeric(diff(t$NDZ_sanemsanas_datums[2:3])),
                           as.numeric(difftime(t$last_date[4], t$NDZ_sanemsanas_datums[4], units = "days")) + 1)
          
        } else {stop("starpkodi4_25: Iztrūkst aptrādes koda.")}
      } else {stop("starpkodi4_25: Iztrūkst aptrādes koda.")}
    } else {stop("starpkodi4_25: Iztrūkst aptrādes koda.")}
  } else {stop("starpkodi4_25: Iztrūkst aptrādes koda.")}
  
  rm(y, t, prev, v)
  return(yt)
}

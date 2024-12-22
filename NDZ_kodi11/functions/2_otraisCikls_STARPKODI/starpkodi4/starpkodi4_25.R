starpkodi4_25 <- function(y, t, prev, v) {
  
  if (t$zk[2] == "51") {
           if (t$zk[3] == "11") {
             if (t$zk[4] == "25") {
               if (diff(t$NDZ_sanemsanas_datums[1:2]) == 0 && diff(t$NDZ_sanemsanas_datums[3:4]) != 0) {
                 yt <- y[v, ]
                 yt$dd <- as.numeric(diff(t$NDZ_sanemsanas_datums[3:4]))
               } else {stop("1starpkodi4_25: Iztrūkst aptrādes koda.")}
             } else {stop("2starpkodi4_25: Iztrūkst aptrādes koda.")}
           } else if (t$zk[3] == "51") {
               if (t$zk[4] == "51") {
                if (all(diff(t$NDZ_sanemsanas_datums[2:4]) != 0) && diff(t$NDZ_sanemsanas_datums[1:2]) == 0) {
                   yt <- y[v, ]
                   yt$dd <- as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days"))
                } else {stop("7starpkodi4_25: Iztrūkst aptrādes koda.")}
             } else {stop("8starpkodi4_25: Iztrūkst aptrādes koda.")}
           } else {stop("9starpkodi4_25: Iztrūkst aptrādes koda.")}
    
  } else if (t$zk[2] == "11") {
    if (t$zk[3] == "50") {
      if (t$zk[4] == "51") {
        if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
          yt <- y[v, ]
          yt$dd <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")),
                           as.numeric(diff(t$NDZ_sanemsanas_datums[2:3])),
                           as.numeric(difftime(t$last_date[4], t$NDZ_sanemsanas_datums[4], units = "days")) + 1)
          
        } else {stop("starpkodi4_25: Iztrūkst aptrādes koda.")}
      } else if (t$zk[3] == "11") {
        if (t$zk[4] == "50") {
          if (all(sapply(c(1,3), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
              diff(t$NDZ_sanemsanas_datums[2:3]) != 0) {
            if (t$PS_code[1] == '____________' && t$NM_code[1] == '____________') {
              yt <- y[v, ]
              yt$dd <- as.numeric(sum(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days"),
                                          diff(t$NDZ_sanemsanas_datums[3:4])))
            } else {stop("starpkodi4_25: Iztrūkst aptrādes koda.")}
          } else {stop("starpkodi4_25: Iztrūkst aptrādes koda.")}
        } else {stop("starpkodi4_25: Iztrūkst aptrādes koda.")}
    } else {stop("starpkodi4_25: Iztrūkst aptrādes koda.")}
    } else {stop("starpkodi4_25: Iztrūkst aptrādes koda.")}
  } else {stop("starpkodi4_25: Iztrūkst aptrādes koda.")}
  
  rm(y, t, prev, v)
  return(yt)
}

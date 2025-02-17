starpkodi5_51 <- function(y, t, prev, v) {
  
  yt <- y[v,]
  
  if (t$zinkod[2] %in% c("40", "50", "53", "91")) {
          if (t$zinkod[3] %in% c("41", "51", "54", "92")) {
            if (t$zinkod[4] %in% c("21", "22", "23", "24", "25", "29")) {
              if (t$zinkod[5] %in% c("40", "50", "53", "91")) {
                if (all(sapply(c(2,4), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
                    all(sapply(c(1,3), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
                  yt$dienas <- sum(as.numeric(diff(t$NDZ_sanemsanas_datums[1:2])), 
                                   as.numeric(diff(t$NDZ_sanemsanas_datums[3:4])))
                } else {stop("starpkodi5_51: Iztrūkst apstrādes koda.")}
              } else if (t$zinkod[5] %in% c("41", "51", "54", "92")) {
                if (diff(t$NDZ_sanemsanas_datums[4:5]) == 0 && all(diff(t$NDZ_sanemsanas_datums[1:4]) != 0)) {
                  if (t$period[1] == "______" && t$PS_code[1] == "______" && t$NM_code[1] == "______") {
                  yt$dienas <- sum(as.numeric(diff(t$NDZ_sanemsanas_datums[1:2])), 
                                   as.numeric(diff(t$NDZ_sanemsanas_datums[4:5])))
                 } else {stop("starpkodi5_51: Iztrūkst apstrādes koda.")}
                } else {stop("starpkodi5_51: Iztrūkst apstrādes koda.")}
              } else {stop("starpkodi5_51: Iztrūkst apstrādes koda.")}
            } else if (t$zinkod[4] %in% c("40", "50", "53", "91")) {
              if (t$zinkod[5] %in% c("21", "22", "23", "24", "25", "29")) {
                if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
                  yt$dienas <- sum(as.numeric(diff(t$NDZ_sanemsanas_datums[1:2])), 
                                   as.numeric(diff(t$NDZ_sanemsanas_datums[3:4])))
                } else {stop("starpkodi5_51: Iztrūkst apstrādes koda.")}
              } else {stop("starpkodi5_51: Iztrūkst apstrādes koda.")}
            } else {stop("starpkodi5_51: Iztrūkst apstrādes koda.")}
          } else {stop("starpkodi5_51: Iztrūkst apstrādes koda.")}
  } else {stop("starpkodi5_51: Iztrūkst apstrādes koda.")}

  rm(y, t, prev, v)
  return(yt)
}


## else if(t$zinkod[1] == "53" && t$zinkod[2] == "54" && 
##         t$zinkod[3] == "53" && t$zinkod[4] == "54" && 
##         t$zinkod[5] == "25" && 
##         all(diff(t$NDZ_sanemsanas_datums) != 0)) {
## days1 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1 
## days2 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[3], t$NDZ_sanemsanas_datums[2], units = "days")) 
## days3 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[5], t$NDZ_sanemsanas_datums[4], units = "days")) + 1  
## 
## yt <- y[v, ]
## yt$dienas <- sum(days1, days2, days3)
## rm(days1, days2, days3)

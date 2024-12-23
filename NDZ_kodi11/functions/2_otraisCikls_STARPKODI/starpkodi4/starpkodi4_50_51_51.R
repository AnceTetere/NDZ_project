starpkodi4_50_51_51 <- function(y, t, prev, v) {

  yt <- y[v, ]
    
  if (t$zk[4] %in% c("40", "50", "53", "91")) {
           # if (diff(t$NDZ_sanemsanas_datums[1:2]) == 0 && all(diff(t$NDZ_sanemsanas_datums[2:4]) != 0)) {
           #       yt$dd <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1, 
          #                         as.numeric(diff(t$NDZ_sanemsanas_datums[3:4])))
           # } else 
              if (all(sapply(c(1,3), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) && diff(t$NDZ_sanemsanas_datums[2:3]) != 0) {
                if (t$PS_code[1] == "_________" && t$NM_code[1] == "_________") {
                  #Skaidrībai:
                  t <- slice(t, c(2,1,3,4)) 
                  yt$dd <- sum(sapply(c(1,3), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)])))
                } else {stop("starpkodi4_50_51_51: Trūkst izstrādes koda.")}
              } else {stop("starpkodi4_50_51_51: Trūkst izstrādes koda.")}
  } else  if (t$zk[4] %in% c("21", "22", "23", "24", "25", "29")) {
           if (all(sapply(c(1,3), function(i) all(diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) && diff(t$NDZ_sanemsanas_datums[2:3]) == 0) {
             yt$dd <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1, 
                              as.numeric(diff(t$NDZ_sanemsanas_datums[3:4])) + 1)
           } else if (diff(t$NDZ_sanemsanas_datums[1:2]) == 0) {
             yt$dd <- as.numeric(diff(t$NDZ_sanemsanas_datums[3:4])) + 1 
           } else if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
             yt$dd <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1,
                              as.numeric(diff(t$NDZ_sanemsanas_datums[3:4])) + 1)
           } else {stop("starpkodi4_50_51_51: Trūkst izstrādes koda.")}
  } else {stop("starpkodi4_50_51_51: Trūkst izstrādes koda.")}
  
  rm(y, t, prev, v)
  return(yt)
}

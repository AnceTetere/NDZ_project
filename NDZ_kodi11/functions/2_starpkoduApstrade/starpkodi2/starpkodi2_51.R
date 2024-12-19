starpkodi2_51 <- function(y, t, prev, v) {
  
  if (t$zk[2] %in% c("11", "14", "16", "61")) {
           if (t$PS_code[1] == '________' && t$NM_code[1] == '________') {
             yt <- y[v,] 
             yt$dd <- 0
           } else if (t$PS_code[1] == '________' && t$NM_code[1] == '________') {
             yt <- y[v,] 
             yt$dd <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")),
                              as.numeric(difftime(t$last_date[2], t$NDZ_sanemsanas_datums[2], units = "days")))
           } else if (t$PS_code[1] == '________' && t$NM_code[1] == '________' && t$period[1] == "202101") {
             yt <- y[v:(v+1),]
             yt <- yt[yt$zk == "11",]
           } else {stop("starpkodi2_51: Trūkst izstrādes koda.")}
  } else if (t$zk[2] %in% c("21", "22", "23", "24", "25", "29")) {
            yt <- y[v,]
            yt$dd <- as.numeric(diff(t$NDZ_sanemsanas_datums[1:2]) + 1)
  } else if (t$zk[2] %in% c("40", "50", "53", "91")) {
            yt <- y[v,] 
            yt$dd <- as.numeric(diff(t$NDZ_sanemsanas_datums[1:2]))
  #  
  #} else if (t$zk[2] == "41" && t$PS_code[1] == '________' && t$NM_code[1] == '________') {
  #  days1 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days"))
  #  days2 <- as.numeric(difftime(t$last_date[2], t$NDZ_sanemsanas_datums[2], units = "days"))
    
  #  yt <- y[v,] 
  #  yt$dd <- sum(days1, days2)
  #  rm(days1, days2)
  #} else if (t$zk[2] == "92") {
  #  yt <- y[v,] 
  #  yt$dd <- as.numeric(difftime(t$last_date[2], t$NDZ_sanemsanas_datums[2], units = "days")) + 1
  } else {stop("starpkodi2_51: Trūkst izstrādes koda.")}
  
  rm(y, t, prev, v)
  return(yt)
}

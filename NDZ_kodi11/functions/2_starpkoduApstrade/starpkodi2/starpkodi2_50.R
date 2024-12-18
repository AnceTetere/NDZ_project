starpkodi2_50 <- function(y, t, prev, v) {
  
  if (t$zk[2] == "25") {
        if (diff(t$NDZ_sanemsanas_datums[1:2]) != 0) {
          yt <- y[v:(v+1),] %>% dplyr::filter(zk == "50")
        } else {stop("Starpkodi2_50: Trūkst izstrādes koda.")}
  } else if (t$zk[2] == "21") {
        if (diff(t$NDZ_sanemsanas_datums[1:2]) != 0) {
          yt <- y[v:(v+1),] %>% filter(zk == "50")
        } else {stop("Starpkodi2_50: Trūkst izstrādes koda.")}
  } else if (t$zk[2] == "22") {
      if (diff(t$NDZ_sanemsanas_datums[1:2]) != 0) {
         yt <- y[v,]
         yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[t$zk == "50"], prev, units = "days")) - 1 
    } else {stop("Starpkodi2_50: Trūkst izstrādes koda pāra kodam 22.")}
  #} else if (t$zk[2] == "24" && diff(t$NDZ_sanemsanas_datums) != 0) {
  #  yt <- y2[v,] 
  #  yt$dienas <- as.numeric(difftime(t$beidz[1], prev, units = "days")) - 1 
  #} else if (t$zk[2] == "41" && diff(t$NDZ_sanemsanas_datums) != 0 &&
  #           t$PS_code[1] == '____________' && t$NM_code[1] == '____________') {
  #  days1 <- as.numeric(difftime(t$beidz[1], prev, units = "days")) - 1
  #  days2 <- as.numeric(difftime(t$last_date[2], t$sak[2], units = "days")) + 1
    
  #  yt <- y2[v,] 
  #  yt$dienas <- sum(days1, days2)
  #  rm(days1, days2)
  #} else if (t$zk[2] == "53" && diff(t$NDZ_sanemsanas_datums) != 0 &&
  #           t$PS_code[1] == '____________' && t$NM_code[1] == '____________') {
  #  yt <- y2[v,] 
  #  yt$dienas <- as.numeric(difftime(t$beidz[2], prev, units = "days")) - 1
  #} else if (t$zk[2] == "11" && diff(t$NDZ_sanemsanas_datums) != 0 &&
  #           t$PS_code[1] == '____________' && t$NM_code[1] == '____________') {
  #  days1 <- as.numeric(difftime(t$beidz[1], prev, units = "days"))
  #  days2 <- as.numeric(difftime(t$last_date[2], t$sak[2], units = "days")) + 1
    
  #  yt <- y2[v,] 
  #  yt$dienas <- sum(days1, days2)
  #  rm(days1, days2)
  } else {stop("Starpkodi2_50: Trūkst izstrādes koda.")}
  
  if(is.na(yt$PS_code[1])) {stop("Dienas NA.")}
  return(yt)
}

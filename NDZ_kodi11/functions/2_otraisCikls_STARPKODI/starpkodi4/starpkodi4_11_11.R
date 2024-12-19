starpkodi4_11_11 <- function(y2, t, prev, v) {
  
  if (t$zk[3] == "50" && t$zk[4] == "51" && all(diff(t$NDZ_sanemsanas_datums) != 0)) {
    yt <- y2[v, ]
    yt$dd <- as.numeric(sum(diff(t$NDZ_sanemsanas_datums[2:3]),
                     difftime(t$last_date[4], t$NDZ_sanemsanas_datums[4], units = "days")) + 1)
  #} else if (t$zk[3] == "40" && t$zk[4] == "11" && all(diff(t$NDZ_sanemsanas_datums[2:4]) != 0) && diff(t$NDZ_sanemsanas_datums[1:2]) == 0 &&
  #           t$PS_code[1] == '___________' && t$NM_code[1] == '___________') {
  #  yt <- y2[v, ]
  #  yt$dd <- as.numeric(difftime(t$NDZ_sanemsanas_datums[3], t$NDZ_sanemsanas_datums[2], units = "days"))
  #} else if (t$zk[3] == "50" && t$zk[4] == "51" &&all(diff(t$NDZ_sanemsanas_datums[2:4]) != 0) && diff(t$NDZ_sanemsanas_datums[1:2]) == 0) {
   # days1 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[3], t$NDZ_sanemsanas_datums[2], units = "days"))
  #  days2 <- as.numeric(difftime(t$last_date[4], t$NDZ_sanemsanas_datums[4], units = "days")) + 1
    
   # yt <- y2[v, ]
  #  yt$dd <- sum(days1, days2)
   # rm(days1,days2)
  #} else if (t$zk[3] == "50" && t$zk[4] == "51" && all(diff(t$NDZ_sanemsanas_datums[1:3]) == 0) && diff(t$NDZ_sanemsanas_datums[3:4]) != 0) {
   # days1 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[3], t$NDZ_sanemsanas_datums[2], units = "days"))
  #  days2 <- as.numeric(difftime(t$last_date[4], t$NDZ_sanemsanas_datums[4], units = "days")) + 1
    
   # yt <- y2[v, ]
  #  yt$dd <- sum(days1, days2)
   # rm(days1,days2)
  #} else if (t$zk[3] == "40" && t$zk[4] == "40" && all(diff(t$NDZ_sanemsanas_datums) == 0)) {
  #  yt <- y2[v, ]
  #  yt$dd <- as.numeric(difftime(t$NDZ_sanemsanas_datums[4], t$NDZ_sanemsanas_datums[1], units = "days"))
  } else {
    stop("Starpkodi4_11_11: Trūkst izstrādes koda.")
  }
  
  rm(y2, t, prev, v)
  return(yt) 
}

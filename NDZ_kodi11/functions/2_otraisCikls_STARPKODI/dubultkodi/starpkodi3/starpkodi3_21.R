starpkodi3_21 <- function(y2, t, prev, v) {
  
  if (t$zinkod[2] == "50" && t$zinkod[3] == "51" && all(diff(t$NDZ_sanemsanas_datums) == 0)) {
    yt <- y2[v, ]
    yt$dienas <- as.numeric(difftime(t$beidz[1], prev, units = "days"))
  } else if (t$zinkod[2] == "50" && t$zinkod[3] == "51" && all(diff(t$NDZ_sanemsanas_datums[2:3]) == 0) &&
              all(diff(t$NDZ_sanemsanas_datums[1:2]) != 0)) {
    yt <- y2[v:(v+1), ]
    yt <- yt[yt$zinkod == "11", ]
  } else if (t$zinkod[2] == "50" && t$zinkod[3] == "51" && all(diff(t$NDZ_sanemsanas_datums) != 0)) {
    yt <- y2[v:(v+1), ]
    yt <- yt[yt$zinkod == "11", ]
  } else if (t$zinkod[2] == "50" && t$zinkod[3] == "51" && all(diff(t$NDZ_sanemsanas_datums[2:3]) != 0) && all(diff(t$NDZ_sanemsanas_datums[1:2]) == 0)) {
    yt <- y2[v:(v+1), ]
    yt <- yt[yt$zinkod == "11", ]
  } else if (t$zinkod[2] == "51" && t$zinkod[3] == "51" && all(diff(t$NDZ_sanemsanas_datums[2:3]) != 0) && all(diff(t$NDZ_sanemsanas_datums[1:2]) == 0)) {
    yt <- y2[v:(v+1), ]
    yt <- yt[yt$zinkod == "11", ]
  } else if (t$zinkod[2] == "51" && t$zinkod[3] == "11" && 
             diff(t$NDZ_sanemsanas_datums[2:3]) != 0 && diff(t$NDZ_sanemsanas_datums[1:2]) == 0) {
    yt <- y2[v, ]
    yt$dienas <- as.numeric(difftime(t$last_date[3], t$sak[3], units = "days"))
  } else if (t$zinkod[2] == "92" && t$zinkod[3] == "41" && all(diff(t$NDZ_sanemsanas_datums) == 0)) {
    yt <- y2[v, ]
    yt$dienas <- 0
  } else {
    stop("Starpkodi3_21: Trūkst izstrādes koda.")
  }
  
  if(is.na(yt$PS_code[1])) {stop("Dienas NA.")}
  return(yt) 
}


starpkodi3_11 <- function(y2, t, prev, v) {
  
  if (t$zinkod[2] == "50"){
    yt <- starpkodi3_11_50(y2, t, prev, v)
  } else if (t$zinkod[2] == "53") {
    yt <- starpkodi3_11_53(y2, t, prev, v)
  } else if (t$zinkod[2] %in% c("21", "25")) {
    yt <- starpkodi3_11_25(y2, t, prev, v)
  } else if (t$zinkod[2] %in% c("51", "41")) {
    yt <- starpkodi3_11_51(y2, t, prev, v)
  } else if (all(t$zinkod[2:3] == "40") && t$PS_code[1] == '____________' && t$NM_code[1] == '____________') {
    days1 <- as.numeric(difftime(t$beidz[2], t$sak[1], units = "days"))
    days2 <- as.numeric(difftime(t$last_date[3], t$beidz[3], units = "days")) + 1
    
    yt <- y2[v, ]
    yt$dienas <- sum(days1, days2)
    rm(days1, days2)
  } else if (t$zinkod[2] == "91" && t$zinkod[3] == "92" && all(!diff(t$NDZ_sanemsanas_datums) == 0)) {
    days1 <- as.numeric(difftime(t$beidz[2], t$sak[1], units = "days"))
    days2 <- as.numeric(difftime(t$last_date[3], t$sak[3], units = "days")) + 1 
    
    yt <- y2[v, ]
    yt$dienas <- sum(days1, days2)
    rm(days1, days2)
  } else if ((t$zinkod[2] == "91" && t$zinkod[3] == "92" && all(diff(t$NDZ_sanemsanas_datums[2:3]) != 0) && all(diff(t$NDZ_sanemsanas_datums[1:2]) == 0)) 
             || 
             (t$zinkod[2] == "40" && t$zinkod[3] == "41" && all(diff(t$NDZ_sanemsanas_datums[2:3]) != 0) && all(diff(t$NDZ_sanemsanas_datums[1:2]) == 0))) {
    yt <- y2[v, ]
    yt$dienas <- as.numeric(difftime(t$last_date[3], t$sak[3], units = "days")) + 1 
  } else if (t$zinkod[2] == "11" && t$zinkod[3] == "91" && all(diff(t$NDZ_sanemsanas_datums) != 0)) {
    yt <- y2[v, ]
    yt$dienas <- as.numeric(difftime(t$beidz[3], t$sak[2], units = "days"))
  } else if (t$zinkod[2] == "40" && t$zinkod[3] == "91" && all(diff(t$NDZ_sanemsanas_datums) == 0)) {
    yt <- y2[v, ]
    yt$dienas <- 0
  } else if (t$zinkod[2] == "40" && t$zinkod[3] == "25" && diff(t$NDZ_sanemsanas_datums[1:2]) == 0 &&
             diff(t$NDZ_sanemsanas_datums[2:3]) != 0) {
    yt <- y2[v, ]
    yt$dienas <- 0
  } else if (t$zinkod[2] == "11" && t$zinkod[3] == "50" && all(diff(t$NDZ_sanemsanas_datums) != 0)) {
    yt <- y2[v, ]
    yt$dienas <- as.numeric(difftime(t$beidz[3], t$sak[2], units = "days"))
  } else if (t$zinkod[2] == "11" && t$zinkod[3] %in% c("40", "50")) {
    yt <- y2[v, ]
    yt$dienas <- as.numeric(difftime(t$beidz[3], t$sak[2], units = "days"))
  } else if (all(t$zinkod[2:3] == "40") && diff(t$NDZ_sanemsanas_datums[2:3]) == 0) {
    yt <- y2[v, ]
    yt$dienas <- as.numeric(difftime(t$beidz[3], t$sak[1], units = "days"))
  } else {
    stop("Starpkodi3_11: Trūkst izstrādes koda.")
  }
  
  if(is.na(yt$PS_code[1])) {stop("Dienas NA.")}
  return(yt) 
}

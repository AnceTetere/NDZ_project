starpkodi3 <- function(y2, t, prev, v) {
  
  if (t$zinkod[1] == "50"){
    yt <- starpkodi3_50(y2, t, prev, v)
  } else if (t$zinkod[1] == "25") {
    yt <- starpkodi3_25(y2, t, prev, v)
  } else if (t$zinkod[1] == "11") {  
    yt <- starpkodi3_11(y2, t, prev, v)
  } else if (t$zinkod[1] == "91") {  
    yt <- starpkodi3_91(y2, t, prev, v)
  } else if (t$zinkod[1] == "53") {  
    yt <- starpkodi3_53(y2, t, prev, v)
  } else if (t$zinkod[1] == "51") {  
    yt <- starpkodi3_51(y2, t, prev, v)
  } else if (t$zinkod[1] == "41") {  
    yt <- starpkodi3_41(y2, t, prev, v)
  } else if (t$zinkod[1] == "21") {  
    yt <- starpkodi3_21(y2, t, prev, v)
  } else if (t$zinkod[1] == "40" && t$zinkod[2] == "21" &&
               t$zinkod[3] == "41" && 
               t$NDZ_sanemsanas_datums[1] != t$NDZ_sanemsanas_datums[2] &&
               t$NDZ_sanemsanas_datums[2] == t$NDZ_sanemsanas_datums[3]) {
      yt <- y2[v, ]
      yt$dienas <- as.numeric(difftime(t$beidz[1], prev, units = "days")) - 1 
    } else if (t$zinkod[1] == "40" && t$zinkod[2] == "25" &&
               t$zinkod[3] == "41" && 
               all(!diff(t$NDZ_sanemsanas_datums[1:2]) == 0)&&
               t$NDZ_sanemsanas_datums[2] == t$NDZ_sanemsanas_datums[3]) {
      yt <- y2[v, ]
      yt$dienas <- as.numeric(difftime(t$beidz[1], prev, units = "days")) - 1 
    } else if (t$zinkod[1] == "92" && t$zinkod[2] == "50" && t$zinkod[3] == "51" && 
                all(!diff(t$NDZ_sanemsanas_datums) == 0)) {
        days1 <- as.numeric(difftime(t$beidz[2], t$sak[1], units = "days"))
        days2 <- as.numeric(difftime(t$last_date[3], t$sak[3], units = "days")) + 1 
      
      yt <- y2[v, ]
      yt$dienas <- sum(days1, days2)
      rm(days1, days2)
    } else if (t$zinkod[1] == "92" && t$zinkod[2] == "25" && t$zinkod[3] == "11" && all(diff(t$NDZ_sanemsanas_datums) != 0)) {
      days1 <- as.numeric(difftime(t$beidz[2], t$sak[1], units = "days")) + 1  
      days2 <- as.numeric(difftime(t$last_date[3], t$sak[3], units = "days"))  + 1 
      
      yt <- y2[v, ]
      yt$dienas <- sum(days1, days2)
      rm(days1, days2)
    } else if (t$zinkod[1] == "92" && t$zinkod[2] == "50" && t$zinkod[3] == "51" && 
               all(diff(t$NDZ_sanemsanas_datums[2:3]) != 0) && all(diff(t$NDZ_sanemsanas_datums[1:2]) == 0)) {
      yt <- y2[v, ]
      yt$dienas <- as.numeric(difftime(t$last_date[3], t$sak[3], units = "days")) + 1  
    } else if (t$zinkod[1] == "54" && t$zinkod[2] == "53" && t$zinkod[3] == "25" && 
               all(diff(t$NDZ_sanemsanas_datums[1:2]) == 0) &&
               all(diff(t$NDZ_sanemsanas_datums[2:3]) != 0)) {
      yt <- y2[v, ]
      yt$dienas <- 0
    } else if (t$zinkod[1] == "92" && t$zinkod[2] == "41" && t$zinkod[3] == "51" && 
               all(diff(t$NDZ_sanemsanas_datums) == 0)) {
      yt <- y2[v:(v+1), ]
      yt <- yt[yt$zinkod == "40", ]
    } else if (t$zinkod[1] == "21" && t$zinkod[2] == "51" && t$zinkod[3] == "51" && 
               all(diff(t$NDZ_sanemsanas_datums[1:2]) == 0)) {
      yt <- y2[v, ]
      yt$dienas <- 0
    } else if (t$zinkod[1] == "92" && t$zinkod[2] == "41" && t$zinkod[3] == "25" && 
               all(diff(t$NDZ_sanemsanas_datums[2:3]) != 0) && all(diff(t$NDZ_sanemsanas_datums[1:2]) == 0)) {
      yt <- y2[v, ]
      yt$dienas <- as.numeric(difftime(t$beidz[3], t$sak[2], units = "days")) + 1 
    } else if (t$zinkod[1] == "40" && t$zinkod[2] == "41" && t$zinkod[3] == "25" && all(diff(t$NDZ_sanemsanas_datums) != 0)) {
      days1 <- as.numeric(difftime(t$beidz[1], prev, units = "days")) - 1
      days2 <- as.numeric(difftime(t$beidz[3], t$sak[2], units = "days")) + 1
      
      yt <- y2[v, ]
      yt$dienas <- sum(days1, days2)
      rm(days1, days2)
    } else if (t$zinkod[1] == "92" && t$zinkod[2] == "25" && t$zinkod[3] == "92" && 
               diff(t$NDZ_sanemsanas_datums[1:2]) != 0 && diff(t$NDZ_sanemsanas_datums[2:3]) == 0 && 
               t$PS_code[1] == '___________' && t$NM_code[1] == '_____________') {
      yt <- y2[v, ]
      yt$dienas <- as.numeric(difftime(t$sak[1], prev, units = "days")) - 1
    } else {
    stop("Starpkodi3: Trūkst izstrādes koda.")
    }
  
  if(is.na(yt$PS_code[1])) {stop("Dienas NA.")}
  yt$zinkod <- "combined"  #jo starpkodu dienu sarēķins
  return(yt)
}

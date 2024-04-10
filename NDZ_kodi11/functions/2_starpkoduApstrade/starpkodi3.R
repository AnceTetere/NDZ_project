starpkodi3 <- function(y2, t, prev) {
  
  if (t$zinkod[1] == "50"){
    yt <- starpkodi3_50(y2, t, prev)
  } else if (t$zinkod[1] == "25") {
    yt <- starpkodi3_25(y2, t, prev)
    
  } else if (t$zinkod[1] == "11" && t$zinkod[2] == "50" &&
             t$zinkod[3] == "51" && all(diff(t$NDZ_sanemsanas_datums) != 0)) {

    days1 <- as.numeric(difftime(as.Date(t$beidz[2]), as.Date(t$sak[1]), units = "days")) 
    days2 <- as.numeric(difftime(as.Date(t$last_date[3]), as.Date(t$sak[3]), units = "days")) + 1
    days <- days1 + days2 
    rm(days1, days2)
    
    yt <- y2[v, ]
    yt$dienas <- days
  } else if (t$zinkod[1] == "91" && t$zinkod[2] == "92" &&
            t$zinkod[3] == "40" && all(diff(t$NDZ_sanemsanas_datums) != 0)) {

    days1 <- as.numeric(difftime(as.Date(t$beidz[1]), prev, units = "days")) - 1 
    days2 <- as.numeric(difftime(as.Date(t$beidzz[3]), as.Date(t$sak[2]), units = "days")) 
    days <- days1 + days2 
    rm(days1, days2)
    
    yt <- y2[v, ]
    yt$dienas <- days
  #} else if (t$zinkod[1] == "11" && t$zinkod[2] == "53" &&
  #           t$zinkod[3] == "81" && all(diff(t$NDZ_sanemsanas_datums) != 0)) {

  #  days <- as.numeric(difftime(as.Date(t$beidz[2]), as.Date(t$sak[1]), units = "days"))
  #  yt <- y2[v, ]
  #  yt$dienas <- days
  } else if (t$zinkod[1] == "11" && t$zinkod[2] == "53" &&
            t$zinkod[3] == "25" && all(diff(t$NDZ_sanemsanas_datums) != 0)) {

    days <- as.numeric(difftime(as.Date(t$beidz[2]), as.Date(t$sak[1]), units = "days"))
    yt <- y2[v, ]
    yt$dienas <- days
  } else if (t$zinkod[1] == "11" && t$zinkod[2] == "50" &&
             t$zinkod[3] == "51" && all(diff(t$NDZ_sanemsanas_datums) != 0)) {

    days <- as.numeric(difftime(as.Date(t$last_date[3]), as.Date(t$sak[3]), units = "days")) + 1
    yt <- y2[v, ]
    yt$dienas <- days
  } else if (t$zinkod[1] == "11" && t$zinkod[2] == "50" &&
             t$zinkod[3] == "51" && t$NDZ_sanemsanas_datums[1] == t$NDZ_sanemsanas_datums[2]) {
 
    days <- as.numeric(difftime(as.Date(t$last_date[3]), as.Date(t$sak[3]), units = "days")) + 1 
    yt <- y2[v, ]
    yt$dienas <- days
  } else if (t$zinkod[1] == "53" && t$zinkod[2] == "54" &&
            t$zinkod[3] == "21" && all(diff(t$NDZ_sanemsanas_datums) != 0)) {

    days1 <- as.numeric(difftime(as.Date(t$beidz[1]), prev, units = "days")) - 1 
    days2 <- as.numeric(difftime(as.Date(t$beidz[3]), as.Date(t$sak[2]), units = "days")) 
    days <- days1 + days2
    rm(days1, days2)
    
    yt <- y2[v, ]
    yt$dienas <- days
  } else if (t$zinkod[1] == "11" && t$zinkod[2] == "50" &&
             t$zinkod[3] == "25" && all(!diff(t$NDZ_sanemsanas_datums) == 0)) {

    days <- as.numeric(difftime(as.Date(t$beidz[2]), as.Date(t$sak[1]), units = "days"))
    
    yt <- y2[v, ]
    yt$dienas <- days
  } else if (t$zinkod[1] == "91" && t$zinkod[2] == "92" &&
             t$zinkod[3] == "25" && all(!diff(t$NDZ_sanemsanas_datums) == 0)) {

    days1 <- as.numeric(difftime(as.Date(t$beidz[1]), prev, units = "days")) -1 
    days2 <- as.numeric(difftime(as.Date(t$beidz[3]), as.Date(t$sak[2]), units = "days")) + 1
    days <- days1 + days2
    rm(days1, days2)
    
    yt <- y2[v, ]
    yt$dienas <- days
  } else if (t$zinkod[1] == "51" && t$zinkod[2] == "50" &&
             t$zinkod[3] == "25" && all(!diff(t$NDZ_sanemsanas_datums) == 0)) {

    yt <- y2[v, ]
    yt$dienas <- as.numeric(difftime(as.Date(t$beidz[2]), as.Date(t$sak[1]), units = "days"))
  #} else if (t$zinkod[1] == "11" && t$zinkod[2] == "81" &&
  #           t$zinkod[3] == "82" && all(diff(t$NDZ_sanemsanas_datums) == 0)) {

  #  yt <- y2[v, ]
  #  yt$dienas <- 1
  } else if (t$zinkod[1] == "51" && t$zinkod[2] == "50" &&
             t$zinkod[3] == "21" && all(!diff(t$NDZ_sanemsanas_datums) == 0)) {

    yt <- y2[v, ]
    yt$dienas <- as.numeric(difftime(as.Date(t$beidz[2]), as.Date(t$sak[1]), units = "days"))
  } else if (t$zinkod[1] == "11" && t$zinkod[2] == "53" &&
             t$zinkod[3] == "21" && all(!diff(t$NDZ_sanemsanas_datums) == 0)) {

    yt <- y2[v, ]
    yt$dienas <- as.numeric(difftime(as.Date(t$beidz[2]), as.Date(t$sak[1]), units = "days"))
  } else if (t$zinkod[1] == "91" && t$zinkod[2] == "25" &&
             t$zinkod[3] == "92" && all(!diff(t$NDZ_sanemsanas_datums[1:2]) == 0) &&
             t$NDZ_sanemsanas_datums[3] == t$NDZ_sanemsanas_datums[2]) {

    yt <- y2[v, ]
    yt$dienas <- as.numeric(difftime(as.Date(t$beidz[1]), prev, units = "days")) - 1
  } else if (t$zinkod[1] == "91" && t$zinkod[2] == "92" &&
             t$zinkod[3] == "50" && all(!diff(t$NDZ_sanemsanas_datums) == 0)) {

    days1 <- as.numeric(difftime(as.Date(t$beidz[1]), prev, units = "days")) - 1
    days2 <- as.numeric(difftime(as.Date(t$beidz[3]), as.Date(t$sak[2]), units = "days"))
    days <- days1 + days2
    rm(days1, days2)
    
    yt <- y2[v, ]
    yt$dienas <- days
  } else if (t$zinkod[1] == "11" && t$zinkod[2] == "53" &&
             t$zinkod[3] == "25" && all(!diff(t$NDZ_sanemsanas_datums[2:3]) == 0) &&
             t$NDZ_sanemsanas_datums[1] == t$NDZ_sanemsanas_datums[2]) {

    yt <- y2[v, ]
    yt$dienas <- 1
  } else if (t$zinkod[1] == "21" && t$zinkod[2] == "50" &&
              t$zinkod[3] == "51" && all(diff(t$NDZ_sanemsanas_datums) == 0)) {

    yt <- y2[v, ]
    yt$dienas <- as.numeric(difftime(t$beidz[1], prev, units = "days"))
  #} else if (t$zinkod[1] == "11" && t$zinkod[2] == "81" &&
  #           t$zinkod[3] == "25" && all(diff(t$NDZ_sanemsanas_datums[1:2]) == 0) &&
  #           t$NDZ_sanemsanas_datums[2] != t$NDZ_sanemsanas_datums[3]) {

  #  yt <- y2[v:(v+1), ]
  #  yt <- yt[yt$zinkod == "11", ]
  #} else if (t$zinkod[1] == "26" && t$zinkod[2] == "11" &&
  #           t$zinkod[3] == "82" && all(diff(t$NDZ_sanemsanas_datums) == 0)) {

  #  yt <- y2[v, ]
  #  yt$dienas <- 1
  } else if (t$zinkod[1] == "11" && t$zinkod[2] == "25" &&
             t$zinkod[3] == "53" && all(diff(t$NDZ_sanemsanas_datums[2:3]) == 0) &&
             t$NDZ_sanemsanas_datums[1] != t$NDZ_sanemsanas_datums[2]) {

    yt <- y2[v, ]
    yt$dienas <- as.numeric(difftime(t$beidz[3], t$sak[1], units = "days"))
  } else if (t$zinkod[1] == "53" && t$zinkod[2] == "54" &&
             t$zinkod[3] == "50" && all(!diff(t$NDZ_sanemsanas_datums) == 0)) {

    days1 <- as.numeric(difftime(t$beidz[1], prev, units = "days")) - 1 
    days2 <- as.numeric(difftime(t$beidz[3], t$sak[2], units = "days"))
    days <- days1 + days2
    rm(days1, days2)
    
    yt <- y2[v, ]
    yt$dienas <- days
  #} else if (t$zinkod[1] == "81" && t$zinkod[2] == "82" &&
  #           t$zinkod[3] == "50" && all(!diff(t$NDZ_sanemsanas_datums[2:3]) == 0) &&
  #           t$NDZ_sanemsanas_datums[1] == t$NDZ_sanemsanas_datums[2] && 
  #           t$PS_code[1] == 'xxx' && t$NM_code[1] == 'xxx') {
 
  #  yt <- y2[v, ]
  #  yt$dienas <- as.numeric(difftime(t$beidz[3], prev, units = "days")) - 1 
  } else if (t$zinkod[1] == "41" && t$zinkod[2] == "50" &&
             t$zinkod[3] == "51" && all(!diff(t$NDZ_sanemsanas_datums[2:3]) == 0) &&
             t$NDZ_sanemsanas_datums[1] == t$NDZ_sanemsanas_datums[2]) {

    yt <- y2[v, ]
    yt$dienas <- as.numeric(difftime(t$last_date[3], t$sak[3], units = "days")) + 1 
    } else if (t$zinkod[1] == "11" && t$zinkod[2] == "53" &&
               t$zinkod[3] == "54" && all(!diff(t$NDZ_sanemsanas_datums) == 0)) {

      days1 <- as.numeric(difftime(t$beidz[2], t$sak[1], units = "days"))
      days2 <- as.numeric(difftime(t$last_date[3], t$sak[3], units = "days")) + 1 
      days <- days1 + days2
      rm(days1, days2)
      
      yt <- y2[v, ]
      yt$dienas <- days
    #} else if (t$zinkod[1] == "11" && t$zinkod[2] == "81" &&
    #           t$zinkod[3] == "82" && all(diff(t$NDZ_sanemsanas_datums[1:2]) == 0) &&
    #           t$NDZ_sanemsanas_datums[2] != t$NDZ_sanemsanas_datums[3] &&
    #           t$PS_code[1] == 'xxx' && t$NM_code[2] == 'xxx') {

    #  yt <- y2[v:(v+1), ]
    #  yt <- yt[yt$zinkod == "81", ]
    } else if (t$zinkod[1] == "40" && t$zinkod[2] == "21" &&
               t$zinkod[3] == "41" && 
               t$NDZ_sanemsanas_datums[1] != t$NDZ_sanemsanas_datums[2] &&
               t$NDZ_sanemsanas_datums[2] == t$NDZ_sanemsanas_datums[3]) {

      yt <- y2[v, ]
      yt$dienas <- as.numeric(difftime(t$beidz[1], prev, units = "days")) - 1 
    #} else if (t$zinkod[1] == "11" && t$zinkod[2] == "25" &&
    #           t$zinkod[3] == "81" && 
    #           all(diff(t$NDZ_sanemsanas_datums) == 0) &&
    #           all(t$PS_code == 'xxx') && all(t$NM_code == 'xxx')) {

    #  yt <- y2[v, ]
    #  yt$dienas <- 1
    #} else if (t$zinkod[1] == "11" && t$zinkod[2] == "81" &&
    #           t$zinkod[3] == "82" && 
    #           all(diff(t$NDZ_sanemsanas_datums[1:2]) == 0) &&
    #           t$NDZ_sanemsanas_datums[2] != t$NDZ_sanemsanas_datums[3] &&
    #           all(t$PS_code == 'xxx') && all(t$NM_code == 'xxx')) {

    #  
    #  yt <- y2[v, ]
    #  yt$dienas <- as.numeric(difftime(t$beidz[3], t$sak[1], units = "days")) + 1
    } else if (t$zinkod[1] == "11" && t$zinkod[2] == "50" &&
               t$zinkod[3] == "21" && all(!diff(t$NDZ_sanemsanas_datums) == 0)) {

      yt <- y2[v, ]
      yt$dienas <- as.numeric(difftime(t$beidz[2], t$sak[1], units = "days"))
    #} else if (t$zinkod[1] == "11" && t$zinkod[2] == "26" &&
    #           t$zinkod[3] == "82" && all(diff(t$NDZ_sanemsanas_datums) == 0)) {

    #  yt <- y2[v, ]
    #  yt$dienas <- 0
    #} else if (t$zinkod[1] == "26" && t$zinkod[2] == "11" &&
    #           t$zinkod[3] == "81" && all(diff(t$NDZ_sanemsanas_datums) == 0)) {

    #  yt <- y2[v, ]
    #  yt$dienas <- 0
    } else if (t$zinkod[1] == "11" && t$zinkod[2] == "50" &&
               t$zinkod[3] == "21" && 
               t$NDZ_sanemsanas_datums[1] == t$NDZ_sanemsanas_datums[2] &&
               t$NDZ_sanemsanas_datums[2] != t$NDZ_sanemsanas_datums[3]) {

      yt <- y2[v, ]
      yt$dienas <- 0
    #} else if (t$zinkod[1] == "11" && t$zinkod[2] == "26" &&
    #           t$zinkod[3] == "81" && 
    #           all(diff(t$NDZ_sanemsanas_datums) == 0) &&
    #           all(t$PS_code == 'xxx') && all(t$NM_code == 'xxx')) {

    #  yt <- y2[v, ]
    #  yt$dienas <- as.numeric(difftime(t$last_date[3], t$sak[3], units = "days")) + 1
    #} else if (t$zinkod[1] == "81" && t$zinkod[2] == "25" &&
    #          t$zinkod[3] == "82" && 
    #          all(diff(t$NDZ_sanemsanas_datums[2:3]) == 0) &&
    #          t$NDZ_sanemsanas_datums[1] != t$NDZ_sanemsanas_datums[2] && 
    #          all(t$PS_code == 'xxx') && all(t$NM_code == 'xxx')) {

    #  yt <- y2[v, ]
    #  yt$dienas <- as.numeric(difftime(t$beidz[3], prev, units = "days"))
    } else if (t$zinkod[1] == "51" && t$zinkod[2] == "50" &&
              t$zinkod[3] == "25" && 
              all(!diff(t$NDZ_sanemsanas_datums[2:3]) == 0) &&
              t$NDZ_sanemsanas_datums[1] == t$NDZ_sanemsanas_datums[2]) {

      days1 <- as.numeric(difftime(t$beidz[2], prev, units = "days")) - 1 
      days2 <- as.numeric(difftime(t$beidz[3], t$sak[1], units = "days"))
      days <- sum(days1, days2)
      rm(days1, days2)
      
      yt <- y2[v, ]
      yt$dienas <- days
    } else if (t$zinkod[1] == "53" && t$zinkod[2] == "54" &&
               t$zinkod[3] == "25" && 
               all(!diff(t$NDZ_sanemsanas_datums) == 0)) {

      days1 <- as.numeric(difftime(t$beidz[1], prev, units = "days")) - 1
      days2 <- as.numeric(difftime(t$beidz[3], t$sak[2], units = "days")) + 1
      days <- sum(days1, days2)
      rm(days1, days2)
      
      yt <- y2[v, ]
      yt$dienas <- days
    } else if (t$zinkod[1] == "40" && t$zinkod[2] == "25" &&
               t$zinkod[3] == "41" && 
               all(!diff(t$NDZ_sanemsanas_datums[1:2]) == 0)&&
               t$NDZ_sanemsanas_datums[2] == t$NDZ_sanemsanas_datums[3]) {

      yt <- y2[v, ]
      yt$dienas <- as.numeric(difftime(t$beidz[1], prev, units = "days")) - 1
    } else if (t$zinkod[1] == "11" && t$zinkod[2] == "21" &&
               t$zinkod[3] == "50" && 
               all(!diff(t$NDZ_sanemsanas_datums[1:2]) == 0) &&
               t$NDZ_sanemsanas_datums[2] == t$NDZ_sanemsanas_datums[3]) {

      yt <- y2[v, ]
      yt$dienas <- as.numeric(difftime(t$beidz[2], t$sak[1], units = "days"))
    } else if (t$zinkod[1] == "91" && t$zinkod[2] == "92" &&
               t$zinkod[3] == "50" && 
               all(!diff(t$NDZ_sanemsanas_datums[1:2]) == 0) &&
               t$NDZ_sanemsanas_datums[2] == t$NDZ_sanemsanas_datums[3]) {

      yt <- y2[v, ]
      yt$dienas <- as.numeric(difftime(t$beidz[1], prev, units = "days")) - 1 #jo atvaļinājums
    #} else if (t$zinkod[1] == "21" && t$zinkod[2] == "11" &&
    #           t$zinkod[3] == "81" && 
    #           all(!diff(t$NDZ_sanemsanas_datums[1:2]) == 0) &&
    #           t$NDZ_sanemsanas_datums[2] == t$NDZ_sanemsanas_datums[3]) {

    #  days1 <- as.numeric(difftime(t$beidz[1], prev, units = "days"))
    #  days2 <- as.numeric(difftime(t$last_date[3], t$sak[3], units = "days")) + 1 #jo darbs
    #  days <- sum(days1, days2)
    #  rm(days1, days2)
      
    #  yt <- y2[v, ]
    #  yt$dienas <- days
    } else if (t$zinkod[1] == "53" && t$zinkod[2] == "25" &&
        t$zinkod[3] == "54" && 
        all(!diff(t$NDZ_sanemsanas_datums[1:2]) == 0) &&
        t$NDZ_sanemsanas_datums[2] == t$NDZ_sanemsanas_datums[3]) {
 
      yt <- y2[v, ]
      yt$dienas <- as.numeric(difftime(t$beidz[1], prev, units = "days"))
    } else if (t$zinkod[1] == "11" && t$zinkod[2] == "25" &&
               t$zinkod[3] == "50" && 
               all(!diff(t$NDZ_sanemsanas_datums[1:2]) == 0) &&
               t$NDZ_sanemsanas_datums[2] == t$NDZ_sanemsanas_datums[3]) {
 
      yt <- y2[v, ]
      yt$dienas <- as.numeric(difftime(t$beidz[3], t$sak[1], units = "days"))
    #} else if (t$zinkod[1] == "11" && t$zinkod[2] == "81" &&
    #           t$zinkod[3] == "82" && 
    #           all(diff(t$NDZ_sanemsanas_datums[1:2]) == 0) &&
    #           t$NDZ_sanemsanas_datums[2] != t$NDZ_sanemsanas_datums[3] &&
    #           all(t$PS_code == 'xxx') && all(t$NM_code == 'xxx')) {

    #  yt <- y2[v, ]
    #  yt$dienas <- as.numeric(difftime(t$beidz[3], t$sak[1], units = "days")) + 1 
    stop("Starpkodi3: Trūkst izstrādes koda.")
    }
  
  if(is.na(yt$PS_code[1])) {stop("Dienas NA.")}
  yt$zinkod <- "combined"  #jo starpkodu dienu sarēķins
  return(yt)
}

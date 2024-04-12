starpkodi4 <- function(y2, t, prev, v) {
  
  if (t$zinkod[1] == "11") {
    yt <- starpkodi4_11(y2, t, prev, v)
  } else if (t$zinkod[1] == "25" && t$zinkod[2] == "51" && 
             t$NDZ_sanemsanas_datums[1] == t$NDZ_sanemsanas_datums[2] && 
             t$zinkod[3] == "11" && t$zinkod[4] == "25" && 
             t$NDZ_sanemsanas_datums[3] != t$NDZ_sanemsanas_datums[4]) {

    days <- as.numeric(difftime(as.Date(t$beidz[4]), as.Date(t$sak[3]), units = "days")) 
    yt <- y2[v, ]
    yt$dienas <- days
  } else if (t$zinkod[1] == "51" && t$zinkod[2] == "50" && 
             t$zinkod[3] == "25" && t$zinkod[4] == "51" && 
             t$NDZ_sanemsanas_datums[1] != t$NDZ_sanemsanas_datums[2] &&
             t$NDZ_sanemsanas_datums[3] == t$NDZ_sanemsanas_datums[4]) {

    days <- as.numeric(difftime(as.Date(t$beidz[2]), as.Date(t$sak[1]), units = "days")) 

    yt <- y2[v, ]
    yt$dienas <- days
  #} else if (t$zinkod[1] == "11" && t$zinkod[2] == "26" && 
     #        t$zinkod[3] == "81" && t$zinkod[4] == "82" && 
     #        all(diff(t$NDZ_sanemsanas_datums) == 0)) {

    #yt <- y2[v, ]
    #yt$dienas <- 1
  #} else if (t$zinkod[1] == "11" && t$zinkod[2] == "25" && 
  #           t$zinkod[3] == "11" && t$zinkod[4] == "81" && 
  #           t$NDZ_sanemsanas_datums[1] <= t$NDZ_sanemsanas_datums[2] &&
  #           t$NDZ_sanemsanas_datums[3] == t$NDZ_sanemsanas_datums[4]) {

  #  days1 <- as.numeric(difftime(as.Date(t$beidz[2]), as.Date(t$sak[1]), units = "days")) 
  #  days2 <- as.numeric(difftime(as.Date(t$last_date[4]), as.Date(t$sak[4]), units = "days")) + 1
  #  days <- days1 + days2
  #  rm(days1, days2)
    
  #  yt <- y2[v, ]
  #  yt$dienas <- days
  } else if (t$zinkod[1] == "50" && t$zinkod[2] == "51" && 
             t$zinkod[3] == "25" && t$zinkod[4] == "51" && 
             t$NDZ_sanemsanas_datums[1] == t$NDZ_sanemsanas_datums[2] &&
             t$NDZ_sanemsanas_datums[3] == t$NDZ_sanemsanas_datums[4]) {

    yt <- y2[v, ]
    yt$dienas <- 0
  } else if (t$zinkod[1] == "53" && t$zinkod[2] == "54" && 
            t$zinkod[3] == "53" && t$zinkod[4] == "21" && 
            all(!diff(t$NDZ_sanemsanas_datums) == 0)) {

    days1 <- as.numeric(difftime(as.Date(t$beidz[1]), prev, units = "days")) - 1
    days2 <- as.numeric(difftime(as.Date(t$beidz[3]), as.Date(t$sak[2]), units = "days")) 
    days <- days1 + days2
    rm(days1, days2)
    
    yt <- y2[v, ]
    yt$dienas <- days
 # } else if (t$zinkod[1] == "26" && t$zinkod[2] == "11" && 
 #             t$zinkod[3] == "82" && t$zinkod[4] == "81" && 
 #           all(diff(t$NDZ_sanemsanas_datums) == 0)) {

  #  yt <- y2[v, ]
  #  yt$dienas <- 1
  } else if (t$zinkod[1] == "53" && t$zinkod[2] == "54" && 
             t$zinkod[3] == "53" && t$zinkod[4] == "25" && 
             all(!diff(t$NDZ_sanemsanas_datums) == 0)) {

    days1 <- as.numeric(difftime(as.Date(t$beidz[1]), prev, units = "days")) - 1
    days2 <- as.numeric(difftime(as.Date(t$beidz[3]), as.Date(t$sak[2]), units = "days")) 
    days <- days1 + days2
    rm(days1, days2)
    
    yt <- y2[v, ]
    yt$dienas <- days
  } else if (t$zinkod[1] == "53" && t$zinkod[2] == "54" && 
             t$zinkod[3] == "25" && t$zinkod[4] == "53" && 
             all(!diff(t$NDZ_sanemsanas_datums[1:3]) == 0) &&
             t$NDZ_sanemsanas_datums[3] == t$NDZ_sanemsanas_datums[4]) {

    days1 <- as.numeric(difftime(as.Date(t$beidz[1]), prev, units = "days"))
    days2 <- as.numeric(difftime(as.Date(t$beidz[3]), as.Date(t$sak[2]), units = "days"))
    days <- days1 + days2
    rm(days1, days2)
    
    yt <- y2[v, ]
    yt$dienas <- days
  } else if (t$zinkod[1] == "50" && t$zinkod[2] == "51" && 
             t$zinkod[3] == "25" && t$zinkod[4] == "50" && 
             all(!diff(t$NDZ_sanemsanas_datums[1:3]) == 0) &&
             t$NDZ_sanemsanas_datums[3] == t$NDZ_sanemsanas_datums[4]) {
   
    days1 <- as.numeric(difftime(as.Date(t$beidz[1]), prev, units = "days")) - 1 
    days2 <- as.numeric(difftime(as.Date(t$beidz[3]), as.Date(t$sak[2]), units = "days"))
    days <- days1 + days2
    rm(days1, days2)
    
    yt <- y2[v, ]
    yt$dienas <- days
  } else if (t$zinkod[1] == "50" && t$zinkod[2] == "21" && 
             t$zinkod[3] == "51" && t$zinkod[4] == "11" && 
             t$NDZ_sanemsanas_datums[1] != t$NDZ_sanemsanas_datums[2] &&
             t$NDZ_sanemsanas_datums[2] == t$NDZ_sanemsanas_datums[3] &&
             t$NDZ_sanemsanas_datums[3] != t$NDZ_sanemsanas_datums[4]) {

    days1 <- as.numeric(difftime(as.Date(t$beidz[1]), prev, units = "days")) - 1

    days3 <- as.numeric(difftime(as.Date(t$last_date[4]), as.Date(t$sak[4]), units = "days")) + 1 
    days <- days1 + days2 + days3
    rm(days1, days2, days3)
    
    yt <- y2[v, ]
    yt$dienas <- days
  } else if (t$zinkod[1] == "50" && t$zinkod[2] == "51" && 
            t$zinkod[3] == "21" && t$zinkod[4] == "51" && 
            t$NDZ_sanemsanas_datums[1] == t$NDZ_sanemsanas_datums[2] &&
            t$NDZ_sanemsanas_datums[2] != t$NDZ_sanemsanas_datums[3] &&
            t$NDZ_sanemsanas_datums[3] == t$NDZ_sanemsanas_datums[4]) {

    yt <- y2[v, ]
    yt$dienas <- 0
  #} else if (t$zinkod[1] == "11" && t$zinkod[2] == "26" && 
  #           t$zinkod[3] == "82" && t$zinkod[4] == "81" && 
  #           all(diff(t$NDZ_sanemsanas_datums) == 0)) {

  #  yt <- y2[v, ]
  #  yt$dienas <- 1
  #} else if (t$zinkod[1] == "11" && t$zinkod[2] == "81" && 
  #           t$zinkod[3] == "25" && t$zinkod[4] == "81" && 
  #           all(diff(t$NDZ_sanemsanas_datums[1:2]) == 0) &&
  #           t$NDZ_sanemsanas_datums[3] == t$NDZ_sanemsanas_datums[4]) {

  #  yt <- y2[v:(v+1), ]
  #  yt <- yt[yt$zinkod == "11", ]
  #} else if (t$zinkod[1] == "26" && t$zinkod[2] == "11" && 
  #           t$zinkod[3] == "81" && t$zinkod[4] == "82" && 
  #           all(diff(t$NDZ_sanemsanas_datums) == 0)) {

  #  yt <- y2[v, ]
  #  yt$dienas <- 0
  #} else if (t$zinkod[1] == "25" && t$zinkod[2] == "82" && 
   #          t$zinkod[3] == "11" && t$zinkod[4] == "21" && 
    #         all(diff(t$NDZ_sanemsanas_datums[1:2]) == 0) &&
     #        t$NDZ_sanemsanas_datums[3] != t$NDZ_sanemsanas_datums[4]) {

    #yt <- y2[v:(v+1), ]
    #yt <- yt[yt$zinkod == "11", ]
  } else if (t$zinkod[1] == "51" && t$zinkod[2] == "50" && 
             t$zinkod[3] == "51" && t$zinkod[4] == "21" && 
             all(diff(t$NDZ_sanemsanas_datums[1:2]) == 0) &&
             t$NDZ_sanemsanas_datums[3] != t$NDZ_sanemsanas_datums[4]) {

    yt <- y2[v, ]
    yt$dienas <- as.numeric(difftime(t$beidz[4], t$sak[3], units = "days")) + 1 
  } else if (t$zinkod[1] == "51" && t$zinkod[2] == "50" && 
             t$zinkod[3] == "21" && t$zinkod[4] == "51" && 
             all(!diff(t$NDZ_sanemsanas_datums[1:2]) == 0) &&
             t$NDZ_sanemsanas_datums[3] == t$NDZ_sanemsanas_datums[4]) {

    yt <- y2[v, ]
    yt$dienas <- as.numeric(difftime(t$beidz[2], t$sak[1], units = "days"))
  #} else if (t$zinkod[1] == "50" && t$zinkod[2] == "81" && 
  #          t$zinkod[3] == "51" && t$zinkod[4] == "82" && 
  #          all(diff(t$NDZ_sanemsanas_datums[1:2]) == 0) &&
  #          t$NDZ_sanemsanas_datums[2] != t$NDZ_sanemsanas_datums[3] &&
  #          t$NDZ_sanemsanas_datums[3] != t$NDZ_sanemsanas_datums[4] &&
  #          all(t$pseidokods == 'PKAFE3F4EF6') && all(t$nmrkod == '90011524360')) {

   # yt <- y2[v, ]
    #yt$dienas <- as.numeric(difftime(t$last_date[2], prev, units = "days"))
  } else if (t$zinkod[1] == "91" && t$zinkod[2] == "51" && 
             t$zinkod[3] == "92" && t$zinkod[4] == "50" && 
             all(!diff(t$NDZ_sanemsanas_datums[2:3]) == 0) &&
             t$NDZ_sanemsanas_datums[1] == t$NDZ_sanemsanas_datums[2] &&
             t$NDZ_sanemsanas_datums[3] == t$NDZ_sanemsanas_datums[4]) {

    yt <- y2[v, ]
    yt$dienas <- 0
  } else if (t$zinkod[1] == "51" && t$zinkod[2] == "50" && 
             t$zinkod[3] == "51" && t$zinkod[4] == "25" && 
             all(diff(t$NDZ_sanemsanas_datums) != 0)) {

    days1 <- as.numeric(difftime(as.Date(t$beidz[2]), as.Date(t$sak[1]), units = "days")) 
    days2 <- as.numeric(difftime(as.Date(t$beidz[4]), as.Date(t$sak[3]), units = "days")) + 1 
    days <- (days1 + days2)
    rm(days1, days2)
    
    yt <- y2[v, ]
    yt$dienas <- days
  } else if (t$zinkod[1] == "91" && t$zinkod[2] == "92" && 
             t$zinkod[3] == "50" && t$zinkod[4] == "51" && 
             all(diff(t$NDZ_sanemsanas_datums) != 0)) {

    days1 <- as.numeric(difftime(as.Date(t$beidz[1]), prev, units = "days")) - 1 
    days2 <- as.numeric(difftime(as.Date(t$beidz[3]), as.Date(t$sak[2]), units = "days"))
    days3 <- as.numeric(difftime(as.Date(t$last_date[4]), as.Date(t$sak[4]), units = "days")) + 1 
    
    yt <- y2[v, ]
    yt$dienas <- sum(days1, days2, days3)
    rm(days1, days2, days3)
  } else {
    stop("Starpkodi4 iztrūkst apstrādes koda.")
  }
  
  if(is.na(yt$ps_CODE[1])) {stop("Dienas NA.")}
  yt$zinkod <- "combined"  #jo starpkodu dienu sarēķins
  return(yt)
}

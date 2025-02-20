starpkodi3_51 <- function(y, t, prev, v) {
  
  yt <- y[v, ]

    if (t$zinkod[2] %in% c("40", "50", "53", "91")) {
       yt <- starpkodi3_51_50(y, t, prev, v)
  } else if (t$zinkod[2] %in% c("41", "51", "54", "92")) {
        if (t$zinkod[3] %in% c("21", "22", "23", "24", "25", "29")) {
            if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
              yt$dd <- as.numeric(diff(t$NDZ_sanemsanas_datums[2:3])) + 1 
            } else if (diff(t$NDZ_sanemsanas_datums[1:2]) == 0 && diff(t$NDZ_sanemsanas_datums[2:3]) != 0) {
              yt$dd <- as.numeric(diff(t$NDZ_sanemsanas_datums[2:3])) + 1 
              ZERO_minus(t %>% slice(1,2))
            } else {stop("Starpkodi3_51: Trūkst izstrādes koda.")}
        } else if (t$zinkod[3] %in% c("40", "50", "53", "91")) {
              yt$dd <- as.numeric(diff(t$NDZ_sanemsanas_datums[2:3]))
        } else {stop("Starpkodi3_51: Trūkst izstrādes koda.")}
    } else if (t$zinkod[2] %in% c("21", "22", "23", "24", "25", "29")) {
        yt <- starpkodi3_51_25(y, t, prev, v)          
    } else if (t$zinkod[2] %in% c("11", "14", "16", "61")) {
        yt <- starpkodi3_51_11(y, t, prev, v)          
    } else {stop("Starpkodi3_51: Trūkst izstrādes koda.")}

    rm(y, t, prev, v)
    return(yt) 
  }


#  } else if (t$zinkod[1] == "92" && t$zinkod[2] == "50" && t$zinkod[3] == "51" && 
#                all(!diff(t$NDZ_sanemsanas_datums) == 0)) {
#        dd1 <- as.numeric(difftime(t$beidz_darbu[2], t$sak_darbu[1], units = "dd"))
#        dd2 <- as.numeric(difftime(t$last_date[3], t$sak_darbu[3], units = "dd")) + 1 
#      
#      yt <- y[v, ]
#      yt$dd <- sum(dd1, dd2)
#      rm(dd1, dd2)
#    } else if (t$zinkod[1] == "92" && t$zinkod[2] == "25" && t$zinkod[3] == "11" && all(diff(t$NDZ_sanemsanas_datums) != 0)) {
#      dd1 <- as.numeric(difftime(t$beidz_darbu[2], t$sak_darbu[1], units = "dd")) + 1  
#      dd2 <- as.numeric(difftime(t$last_date[3], t$sak_darbu[3], units = "dd"))  + 1 
#      
#      yt <- y[v, ]
#      yt$dd <- sum(dd1, dd2)
#      rm(dd1, dd2)
#    } else if (t$zinkod[1] == "92" && t$zinkod[2] == "50" && t$zinkod[3] == "51" && 
#               diff(t$NDZ_sanemsanas_datums[2:3]) != 0 && diff(t$NDZ_sanemsanas_datums[1:2]) == 0) {
#      yt <- y[v, ]
#      yt$dd <- as.numeric(difftime(t$last_date[3], t$sak_darbu[3], units = "dd")) + 1  
#    } else if (t$zinkod[1] == "54" && t$zinkod[2] == "53" && t$zinkod[3] == "25" && 
#               diff(t$NDZ_sanemsanas_datums[1:2]) == 0 && diff(t$NDZ_sanemsanas_datums[2:3]) != 0) {
#      yt <- y[v, ]
#      yt$dd <- 0
#    } else if (t$zinkod[1] == "92" && t$zinkod[2] == "41" && t$zinkod[3] == "51" && 
#               all(diff(t$NDZ_sanemsanas_datums) == 0)) {
#      yt <- y[v:(v+1), ]
#      yt <- yt[yt$zinkod == "40", ]    
#    } else if (t$zinkod[1] == "92" && t$zinkod[2] == "41" && t$zinkod[3] == "25" && 
#               diff(t$NDZ_sanemsanas_datums[2:3]) != 0 && diff(t$NDZ_sanemsanas_datums[1:2]) == 0) {
#      yt <- y[v, ]
#      yt$dd <- as.numeric(difftime(t$beidz_darbu[3], t$sak_darbu[2], units = "dd")) + 1 
#    } else if (t$zinkod[1] == "92" && t$zinkod[2] == "25" && t$zinkod[3] == "92" && 
#               diff(t$NDZ_sanemsanas_datums[1:2]) != 0 && diff(t$NDZ_sanemsanas_datums[2:3]) == 0 && 
#               t$PS_code[1] == '_________' && t$NM_code[1] == '_________') {
#      yt <- y[v, ]
#      yt$dd <- as.numeric(difftime(t$sak_darbu[1], prev, units = "dd")) - 1
#    } else if (t$zinkod[1] == "54" && t$zinkod[2] == "25" && t$zinkod[3] == "53" && 
#               diff(t$NDZ_sanemsanas_datums[1:2]) != 0 && diff(t$NDZ_sanemsanas_datums[2:3]) == 0) {
#      yt <- y[v, ]
#      yt$dd <- as.numeric(difftime(t$beidz_darbu[2], t$sak_darbu[1], units = "dd"))

  #} else if (t$zinkod[2] == "91" && t$zinkod[3] == "92" && all(diff(t$NDZ_sanemsanas_datums) != 0)) {
  #  dd1 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[2], t$NDZ_sanemsanas_datums[1], units = "dd"))
  #  dd2 <- as.numeric(difftime(t$last_date[3], t$NDZ_sanemsanas_datums[3], units = "dd"))  + 1 
  #  
  #  yt <- y[v, ]
  #  yt$dd <- sum(dd1, dd2)
  #  rm(dd1, dd2)
  #} else if (t$zinkod[2] == "53" && t$zinkod[3] == "54" && all(diff(t$NDZ_sanemsanas_datums) != 0)) {
  #  dd1 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[2], t$NDZ_sanemsanas_datums[1], units = "dd"))
  #  dd2 <- as.numeric(difftime(t$last_date[3], t$NDZ_sanemsanas_datums[3], units = "dd")) + 1
  #  
  #  yt <- y[v, ]
  #  yt$dd <- sum(dd1,dd2)
  #  rm(dd1,dd2)
  #} else if (t$zinkod[2] == "53" && t$zinkod[3] == "54" && diff(t$NDZ_sanemsanas_datums[2:3]) != 0 &&
  #           diff(t$NDZ_sanemsanas_datums[1:2]) == 0) {
  #  yt <- y[v, ]
  #  yt$dd <- as.numeric(difftime(t$last_date[3], t$NDZ_sanemsanas_datums[3], units = "dd")) + 1  

#if (t$zinkod[2] == "41") {
#  if (t$zinkod[3] == "25") {
#    if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
#      yt <- y[v, ]
#      yt$dd <- as.numeric(diff(t$NDZ_sanemsanas_datums[2:3]))
#    } else {stop("Starpkodi3_41: Trūkst izstrādes koda.")}
#  
#} else if (t$zinkod[2] == "25") {
#  if (t$zinkod[3] == "41") {
#    if (diff(t$NDZ_sanemsanas_datums[2:3]) == 0 && diff(t$NDZ_sanemsanas_datums[1:2]) != 0) {
#      yt <- y[v, ]
#      yt$dd <- 0
#    } else {stop("Starpkodi3_41: Trūkst izstrādes koda.")}
#  } else {stop("Starpkodi3_41: Trūkst izstrādes koda.")}
#  
#} else {stop("Starpkodi3_41: Trūkst izstrādes koda.")}#

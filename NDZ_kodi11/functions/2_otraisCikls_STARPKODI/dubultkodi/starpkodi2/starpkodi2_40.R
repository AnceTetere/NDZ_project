starpkodi2_40 <- function(y, t, prev, v) {

  yt <- y[v,]
  
  if (t$zinkod[2] %in% c("21", "22", "23", "24", "25", "29")) {
      yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1 
  } else if (t$zinkod[2] %in% c("41", "54", "51", "92")) {
    
      if (t$PS_code[1] == '__________' && t$NM_code[1] == '__________') {
           yt$dienas <- as.numeric(sum(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days") - 1, 
                                       difftime(t$last_date[2], t$NDZ_sanemsanas_datums[2], units = "days") + 1))
      } else if (t$period[1] == '_____' && t$PS_code[1] == '__________' && t$NM_code[1] == '__________') {
            yt$dienas <- as.numeric(difftime(t$last_date[2], t$NDZ_sanemsanas_datums[2], units = "days")) + 1
            ZERO_minus(t %>% slice(2))
      } else {stop("Starpkodi2_40: Trūkst izstrādes koda.")}
  } else if (t$zinkod[2] %in% c("50", "53", "40", "91")) {
            if ((t$period[1] == '_____' && t$PS_code[1] == '__________' && t$NM_code[1] == '__________') ||
                (t$period[1] == '_____' && t$PS_code[1] == '__________' && t$NM_code[1] == '__________')) {
              yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1
              ZERO_plus(t %>% slice(2))
            } else {stop("Starpkodi2_40: Trūkst izstrādes koda.")}
  } else if (t$zinkod[2] %in% c("11", "14", "16", "61")) {
               if(diff(t$NDZ_sanemsanas_datums) != 0) {
                 if(t$period[1] == "_____" && t$PS_code[1] == '__________' && t$NM_code[1] == '__________') {
                   yt$dienas <- as.numeric(sum(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days"),
                                               difftime(t$last_date[2], t$NDZ_sanemsanas_datums[2], units = "days") + 1))
                 } else {stop("Starpkodi2_40: Trūkst izstrādes koda.")}
               } else {stop("Starpkodi2_40: Trūkst izstrādes koda.")}
  } else {stop("Starpkodi2_40: Trūkst izstrādes koda.")}


  rm(y, t, prev, v)
  return(yt)
}

#} else if (t$zinkod[1] == "91" && t$zinkod[2] == "21") {
#  yt <- y[v,] 
#  yt$dienas <- as.numeric(difftime(t$beidz_darbu[1], prev, units = "days")) - 1 


#  } else if (t$zinkod[2] == "50" && diff(t$NDZ_sanemsanas_datums[1:2]) != 0 &&
#             t$PS_code[1] == '__________' && t$NM_code[1] == '__________') {
#    yt <- y[v:(v+1),] 
#    yt <- yt[yt$zinkod == "53", ]
#  } else {
#    stop("starpkodi2_53: Trūkst izstrādes koda starpkodi2.")
#  }
#  
#  if(is.na(yt$PS_code[1])) {stop("Dienas NA.")}
#  return(yt)
#} 
#           t$PS_code[1] == '__________' && t$NM_code[1] == '__________') {
#  yt <- y[v,] 
#  yt$dienas <- as.numeric(difftime(t$beidz_darbu[1], prev, units = "days")) - 1 

#} else if (t$zinkod[1] == "40" && t$zinkod[2] == "51" && t$PS_code[1] == '__________' && t$NM_code[1] == '__________') {
#  days1 <- as.numeric(difftime(t$beidz_darbu[1], prev, units = "days")) -1 
#  days2 <- as.numeric(difftime(t$last_date[2], t$sak_darbu[2], units = "days")) + 1 

#  yt <- y[v,] 
#  yt$dienas <- sum(days1, days2)
#  rm(days1,days2)

#   rm(y, t, prev, v)
#   return(yt)
# 

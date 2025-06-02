starpkodi3_11_50 <- function(y, t, prev, v) {
		

  yt <- y[v, ]
  
  if (t$zinkod[3] %in% c("41", "51", "54", "92")) {
          yt$dienas <- sum(as.numeric(diff(t$NDZ_sanemsanas_datums[1:2])), 
                           as.numeric(difftime(t$last_date[3], t$NDZ_sanemsanas_datums[3], units = "days")) + 1)
  } else if (t$zinkod[3] %in% c("21", "22", "23", "24", "25", "29")) {
        if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
          yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[1:2]))
       } else if (diff(t$NDZ_sanemsanas_datums[1:2]) == 0 && diff(t$NDZ_sanemsanas_datums[2:3]) != 0) {
          yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[1:2]))
        } else {stop("Starpkodi3_11_50: Trūkst izstrādes koda.")}
  } else if (t$zinkod[3] %in% c("40", "50", "53", "91")) {
           # yt$dienas <- 0     
          #} else 
          if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
            yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[3], t$NDZ_sanemsanas_datums[1], units = "days"))
          } else if (diff(t$NDZ_sanemsanas_datums[1:2]) == 0 && diff(t$NDZ_sanemsanas_datums[2:3]) != 0) {
            if ((t$PS_code[1] == "___________"   && t$NM_code[1] == "___________" ) ||
                (t$PS_code[1] == "___________"   && t$NM_code[1] == "___________" ) ||
                (t$period[1] == "______" && t$PS_code[1] == "___________"   && t$NM_code[1] == "___________" ) ||
                (t$period[1] == "______" && t$PS_code[1] == "___________"   && t$NM_code[1] == "___________" )) {
              yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[c(1,3)]))
            } else {stop("Starpkodi3_11_50: Trūkst izstrādes koda.")}
          } else {stop("Starpkodi3_11_50: Trūkst izstrādes koda.")}
  } else if (t$zinkod[3] %in% c("11", "14", "16", "61")) {
           if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
             if ((t$period[1] == "______"  && t$PS_code[1] == "___________"   && t$NM_code[1] == "___________" ) ||
                 (t$period[1] == "______"  && t$PS_code[1] == "___________"   && t$NM_code[1] == "___________" )) {
               yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[1:2]))
             } else {stop("Starpkodi3_11_50: Trūkst izstrādes koda.")}
           } else if (diff(t$NDZ_sanemsanas_datums[1:2]) == 0 && diff(t$NDZ_sanemsanas_datums[2:3]) != 0) {
             if (t$period[1] == "______"  && t$PS_code[1] == "___________"   && t$NM_code[1] == "___________" ) {
               yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[1:2]))
             } else {stop("Starpkodi3_11_50: Trūkst izstrādes koda.")}
           } else {stop("Starpkodi3_11_50: Trūkst izstrādes koda.")}
  } else {stop("Starpkodi3_11_50: Trūkst izstrādes koda.")}
  
  rm(y, t, prev, v)
  return(yt) 
}  
  
### No 53
#starpkodi3_11_53 <- function(y2, t, prev, v) {
#  
#  } else if (t$zinkod[3] == "54" && all(!diff(t$NDZ_sanemsanas_datums) == 0)) {
#    days1 <- as.numeric(difftime(t$beidz_darbu[2], t$sak_darbu[1], units = "days"))
#    days2 <- as.numeric(difftime(t$last_date[3], t$sak_darbu[3], units = "days")) + 1 
#    
#    yt <- y2[v, ]
#    yt$dienas <- sum(days1, days2)
#    rm(days1, days2)
#  } else if (t$zinkod[3] == "54") {
#    days1 <- as.numeric(difftime(t$beidz_darbu[2], t$sak_darbu[1], units = "days"))
#    days2 <- as.numeric(difftime(t$last_date[3], t$sak_darbu[3], units = "days")) + 1
#    
#    yt <- y2[v, ]
#    yt$dienas <- sum(days1, days2)
#    rm(days1, days2)
#  } else {
#    stop("Starpkodi3_11_53: Trūkst izstrādes koda.")
#  }
#  
#  if(is.na(yt$PS_code[1])) {stop("Dienas NA.")}
#  return(yt) 
#}




  ##No 40
      # } else if (t$zinkod[3] == "40"){
      #   if (t$PS_code[1] == "___________"   && t$NM_code[1] == "___________" ) {
      #     yt <- y[v, ]
      #     yt$dienas <- sum(as.numeric(diff(t$NDZ_sanemsanas_datums[1:2])), 
      #                      as.numeric(difftime(t$last_date[3], t$NDZ_sanemsanas_datums[3], units = "days")) + 1)
      #   } else if (diff(t$NDZ_sanemsanas_datums[1:2]) != 0 && diff(t$NDZ_sanemsanas_datums[2:3]) == 0) {
      #     yt <- y[v, ]
      #     yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[c(1,3)])
      #   } else {stop("Starpkodi3_11: Trūkst izstrādes koda.")}
      # } else if (t$zinkod[3] == "91") {
      #   if (all(diff(t$NDZ_sanemsanas_datums) == 0)) {
      #     yt <- y[v, ]
      #     yt$dienas <- 0
      #   } else {stop("Starpkodi3_11: Trūkst izstrādes koda.")}

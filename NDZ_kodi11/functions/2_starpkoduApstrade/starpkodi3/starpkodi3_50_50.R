starpkodi3_50_50 <- function(y, t, prev, v) {
  
 if (t$zinkod[3] %in% c("21", "22", "23", "24", "25", "29")) {
              if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
               yt <- y[v, ]
               yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[2], prev, units = "days")) - 1
           } else {stop("starpkodi3_50_50: Iztrūkst apstrādes koda.")} 
 } else if (t$zinkod[3] %in% c("41", "51", "54", "92")) {
                  if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
                    if (t$period[1] == '202101' && t$PS_code[1] %in% c('___________', '___________') && t$NM_code[1] %in% c('4__________', '___________')) {
                      yt <- y[v, ]
                      yt$dienas <- as.numeric(sum(difftime(t$NDZ_sanemsanas_datums[2], prev, units = 'days') - 1,
                                                  difftime(t$last_date[3], t$NDZ_sanemsanas_datums[3], units = 'days') + 1))
                    } else if (t$period[1] == '202101' && t$PS_code[1] == '___________' && t$NM_code[1] == '___________') {
                      yt <- y[v, ]
                      yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = 'days') - 1)
                    } else {stop("starpkodi3_50_50: Iztrūkst apstrādes koda.")}
                  } else {stop("starpkodi3_50_50: Iztrūkst apstrādes koda.")} 
    } else {stop("starpkodi3_50_50: Iztrūkst apstrādes koda.")}
  
  rm(y, t, prev, v)
  return(yt)
}


#  if (t$zinkod[3] %in% c("41", "51", "54", "92")) {
#    if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
#      if (t$PS_code[1] == '___________' && t$NM_code[1] == '___________') {
#        t$zinkod[1] <- '92'; t$sak_beidz[1] <- '1'
#        yt <- y[v,]
#        yt$dienas <- sum(as.numeric(diff(t$NDZ_sanemsanas_datums[1:2])),
#                         as.numeric(difftime(t$last_date[3], t$NDZ_sanemsanas_datums[3], units = "days")) + 1)
#      } else if (t$PS_code[1] == '___________' && t$NM_code[1] == '___________') {
#        yt <- y[v,]
#        yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1
#      } else {stop("starpkodi3_50_91: Iztrūkst apstrādes koda.")}
#    } else {stop("starpkodi3_50_91: Iztrūkst apstrādes koda.")}    

#  #} else if (t$zinkod[2] == "40" && t$zinkod[3] == "51" && diff(t$NDZ_sanemsanas_datums[1:2]) != 0 && diff(t$NDZ_sanemsanas_datums[2:3]) == 0) {
#  #  yt <- y[v, ]
#  #  yt$dienas <- as.numeric(difftime(t$sak_beidz[1], prev, units = "days")) - 1
#  #} else if (t$zinkod[2] == "53" && t$zinkod[3] == "54" && 
#  #           diff(t$NDZ_sanemsanas_datums[1:2]) == 0 && diff(t$NDZ_sanemsanas_datums[2:3]) != 0 &&
#  #           t$PS_code[1] == '___________' && t$NM_code[1] == '___________') {
#  #  days1 <- as.numeric(difftime(t$sak_beidz[2], prev, units = "days")) - 1 
#  #  days2 <- as.numeric(difftime(t$last_date[3], t$sak_beidz[3], units = "days")) + 1 
#    
#   # yt <- y[v, ]
#  #  yt$dienas <- sum(days1, days2)
#  #  rm(days1, days2)
#  #} else if (t$zinkod[2] == "53" && t$zinkod[3] == "54" && 
#  #           diff(t$NDZ_sanemsanas_datums[1:2]) == 0 && diff(t$NDZ_sanemsanas_datums[2:3]) != 0 &&
#  #           t$PS_code[1] == '___________' && t$NM_code[1] == '___________') {
#  #  days1 <- as.numeric(difftime(t$sak_beidz[2], prev, units = "days")) - 1 
#  #  days2 <- as.numeric(difftime(t$last_date[3], t$sak_beidz[3], units = "days")) + 1 
#    
#  #  yt <- y[v, ]
#  #  yt$dienas <- sum(days1, days2)
#  #  rm(days1, days2)

##GIT: 20241105
#starpkodi3_50_91 <- function(y, t, prev,v) {
#  
#    #} else if (t$zinkod[3] == "51") {
#    #  if (diff(t$NDZ_sanemsanas_datums[1:2]) != 0 && diff(t$NDZ_sanemsanas_datums[2:3]) == 0) {
#    #    yt <- y[v, ]
#    #    yt$dienas <- as.numeric(difftime(t$sak_beidz[1], prev, units = "days")) - 1 # jo atvaļinājums
#    #  } else {stop("starpkodi3_50: Iztrūkst apstrādes koda.")}
#  } else {stop("starpkodi3_50: Iztrūkst apstrādes koda.")}
#  
#  rm(y, t, prev,v)
#  return(yt)
#}

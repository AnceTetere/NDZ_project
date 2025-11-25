starpkodi3_50_50 <- function(y, t, prev, v) {
  
  yt <- y[v, ]
  
 if (t$zinkod[3] %in% c("21", "22", "23", "24", "25", "29")) {
              if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
               yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[2], prev, units = "days")) - 1
           } else {stop("starpkodi3_50_50: Iztrūkst apstrādes koda.")} 
 } else if (t$zinkod[3] %in% c("41", "51", "54", "92")) {
            if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
                      if (t$period[1] == '______' && t$PS_code[1] %in% c'______________', '______________') && t$NM_code[1] %in% c'______________', '______________')) {
                           yt$dienas <- as.numeric(sum(difftime(t$NDZ_sanemsanas_datums[2], prev, units = 'days') - 1,
                                                       difftime(t$last_date[3], t$NDZ_sanemsanas_datums[3], units = 'days') + 1))
                    } else if (t$period[1] == '______' && t$PS_code[1] ==  '______________' && t$NM_code[1] ==  '______________') {
                                yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = 'days') - 1)
                    } else {stop("starpkodi3_50_50: Iztrūkst apstrādes koda.")}
            } else if (diff(t$NDZ_sanemsanas_datums[1:2]) == 0 && diff(t$NDZ_sanemsanas_datums[2:3]) != 0) {
                      if ((t$period[1] == '______' && t$PS_code[1] ==  '______________' && t$NM_code[1] ==  '______________') ||
                          (t$period[1] == '______' && t$PS_code[1] ==  '______________' && t$NM_code[1] ==  '______________')) {
                           yt$dienas <- as.numeric(sum(difftime(t$NDZ_sanemsanas_datums[2], prev, units = 'days') - 1,
                                                       difftime(t$last_date[3], t$NDZ_sanemsanas_datums[3], units = 'days') + 1))
                    } else {stop("starpkodi3_50_50: Iztrūkst apstrādes koda.")}
            } else if (diff(t$NDZ_sanemsanas_datums[2:3]) == 0 && diff(t$NDZ_sanemsanas_datums[1:2]) != 0) {
                              if ((t$period[1] == '______' && t$PS_code[1] ==  '______________' && t$NM_code[1] ==  '______________') ||
                                  (t$period[1] == '______' && t$PS_code[1] ==  '______________' && t$NM_code[1] ==  '______________')) {
                                  t <- t[c(1,3,2),]; rownames(t) <- NULL
                                  yt$dienas <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = 'days')) - 1,
                                                   as.numeric(diff(t$NDZ_sanemsanas_datums[2:3])))
                             } else {stop("starpkodi3_50_50: Iztrūkst apstrādes koda.")}
                  } else {stop("starpkodi3_50_50: Iztrūkst apstrādes koda.")} 
    } else {stop("starpkodi3_50_50: Iztrūkst apstrādes koda.")}
  
  rm(y, t, prev, v)
  return(yt)
}

#  #} else if (t$zinkod[2] == "40" && t$zinkod[3] == "51" && diff(t$NDZ_sanemsanas_datums[1:2]) != 0 && diff(t$NDZ_sanemsanas_datums[2:3]) == 0) {
#  #  yt <- y[v, ]
#  #  yt$dienas <- as.numeric(difftime(t$sak_beidz[1], prev, units = "days")) - 1
#    
#  
#starpkodi3_50_91 <- function(y, t, prev,v) {
#  
#    #} else if (t$zinkod[3] == "51") {
#    #  if (diff(t$NDZ_sanemsanas_datums[1:2]) != 0 && diff(t$NDZ_sanemsanas_datums[2:3]) == 0) {
#    #    yt <- y[v, ]
#    #    yt$dienas <- as.numeric(difftime(t$sak_beidz[1], prev, units = "days")) - 1 
#    #  } else {stop("starpkodi3_50: Iztrūkst apstrādes koda.")}
#  } else {stop("starpkodi3_50: Iztrūkst apstrādes koda.")}
#  
#  rm(y, t, prev,v)
#  return(yt)
#}
#

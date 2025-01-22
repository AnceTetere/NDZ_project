starpkodi5_50_51 <- function(y, t, prev, v) {
  
  yt <- y[v,]
  
  if (t$zinkod[3] %in% c("40", "50", "53", "91")) {
    yt <- starpkodi5_50_51_50(y, t, prev, v)
  } else if (t$zinkod[3] %in% c("41", "51", "54", "92")) {
         if (t$zinkod[4] == "21") {
           if (t$zinkod[5] %in% c("40", "50", "53", "91")) {
              if (all(sapply(c(1,4), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0) & 
                  diff(t$NDZ_sanemsanas_datums[2:4]) != 0)) {
              if (t$PS_code[1] == '____________' && t$NM_code[1] == '____________') {
                 yt$dienas <- sum(as.numeric(diff(t$NDZ_sanemsanas_datums[1:2])), 
                                 as.numeric(diff(t$NDZ_sanemsanas_datums[3:4])))
               } else {stop("Starpkodi5_50_51: iztrūkst apstrādes koda.")}
            } else {stop("Starpkodi5_50_51: iztrūkst apstrādes koda.")}
          } else {stop("Starpkodi5_50_51: iztrūkst apstrādes koda.")}
         } else if (t$zinkod[4] %in% c("40", "50", "53", "91")) {
           if (t$zinkod[5] %in% c("40", "50", "53", "91")) {
             if (all(sapply(c(1,3), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) && 
                     all(sapply(c(2,4), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
               if (t$period[1] == "____________" && t$PS_code[1] == '____________' && t$NM_code[1] == '____________') {
                 yt$dienas <- sum(as.numeric(diff(t$NDZ_sanemsanas_datums[1:2])), 
                                  as.numeric(diff(t$NDZ_sanemsanas_datums[3:4])),
                                  as.numeric(difftime(t$last_date[5], t$NDZ_sanemsanas_datums[5], units = "days")))
               } else {stop("Starpkodi5_50_51: iztrūkst apstrādes koda.")}
             } else {stop("Starpkodi5_50_51: iztrūkst apstrādes koda.")}
           } else {stop("Starpkodi5_50_51: iztrūkst apstrādes koda.")}
         } else {stop("Starpkodi5_50_51: iztrūkst apstrādes koda.")} 
   } else  if(t$zinkod[3] %in% c("21", "22", "23", "24", "25", "29")) {
           if (t$zinkod[4] %in% c("41", "51", "54", "92")) {
             if (t$zinkod[5]  %in% c("21", "22", "23", "24", "25", "29")) {
               if (all(sapply(c(1,2,4), function(i) t$NDZ_sanemsanas_datums[i:(i+1)] != 0)) && diff(t$NDZ_sanemsanas_datums[3:4]) == 0) {
                 yt$dienas <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1,
                                  as.numeric(diff(t$NDZ_sanemsanas_datums[4:5])) + 1) 
               } else {stop("Starpkodi5_50_51: iztrūkst apstrādes koda.")}
             } else if (t$zinkod[5] == "11") {
               if (all(sapply(c(1,3), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) && diff(t$NDZ_sanemsanas_datums[4:5]) != 0) {
                 yt$dienas <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1,  
                                  as.numeric(difftime(t$last_date[5], t$NDZ_sanemsanas_datums[5], units = "days")) + 1) 
                 
               } else {stop("Starpkodi5_50_51: iztrūkst apstrādes koda.")}
             } else {stop("Starpkodi5_50_51: iztrūkst apstrādes koda.")}
           } else {stop("Starpkodi5_50_51: iztrūkst apstrādes koda.")}
    } else {stop("Starpkodi5_50_51: iztrūkst apstrādes koda.")} 

  rm(y, t, prev, v)
  return(yt)
}

      
             
             
                
    
    #     } else {stop("Starpkodi5_50_51: iztrūkst apstrādes koda.")} 
#  } else if(t$zinkod[3] == "91") {
#    if (t$zinkod[4] == "92") {
#      if (t$zinkod[5] == "40") {
#        if (all(diff(t$NDZ_sanemsanas_datums[1:4]) != 0) && diff(t$NDZ_sanemsanas_datums[4:5]) == 0) {
#          yt <- y[v, ]
#          yt$dienas <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1, 
#                           as.numeric(diff(t$NDZ_sanemsanas_datums[2:3])),
#                           as.numeric(diff(t$NDZ_sanemsanas_datums[4:5])))
#        } else {stop("Starpkodi5_50_51: iztrūkst apstrādes koda.")}
#      } else {stop("Starpkodi5_50_51: iztrūkst apstrādes koda.")}
#    } else {stop("Starpkodi5_50_51: iztrūkst apstrādes koda.")}



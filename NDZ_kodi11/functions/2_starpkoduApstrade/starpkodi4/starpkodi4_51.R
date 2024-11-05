starpkodi4_51 <- function(y, t, prev, v) {
  
  if (t$zinkod[2] %in% c("41", "51", "54", "92")) {
                if (t$zinkod[3] %in% c("40", "50", "53", "91")) {
                  if (t$zinkod[4] %in% c("21", "22", "23", "24", "25", "29")) {
                    if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
                      yt <- y[v:(v+1), ] %>% filter(zinkod == "50")
                    } else {stop("Starpkodi4_51: Trūkst izstrādes koda.")}
                 # } else if (t$zinkod[4] %in% c("41", "51", "54", "92")) {
                 #          if (diff(t$NDZ_sanemsanas_datums[2:3]) == 0 && all(sapply(c(1,3), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
                 #            yt <- y[v,]
                 #            yt$dienas <- as.numeric(sum(diff(t$NDZ_sanemsanas_datums[2:3]), 
                 #                                        difftime(t$last_date[4], t$NDZ_sanemsanas_datums[4], units = "days") + 1))
                 #         } else {stop("Starpkodi4_41 iztrūkst apstrādes koda.")}
                   } else {stop("Starpkodi4_41 iztrūkst apstrādes koda.")}
                } else if (t$zinkod[3] %in% c("21", "22", "23", "24", "25", "29")) {
                         if (t$zinkod[4] %in% c("21", "22", "23", "24", "25", "29"))  {
                          if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
                          yt <- y[v, ]
                          yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[c(2,4)]) + 1)
                    } else {stop("Starpkodi4_51: Trūkst izstrādes koda.")}
                  } else {stop("Starpkodi4_51: Trūkst izstrādes koda.")}
                } else {stop("Starpkodi4_51: Trūkst izstrādes koda.")}
  } else if (t$zinkod[2] %in% c("21", "22", "23", "24", "25", "29")) {
               if (t$zinkod[3] %in% c("41", "51", "54", "92")) {
                 if (t$zinkod[4] %in% c("41", "51", "54", "92")) {
                   if (all(sapply(c(1,3), function(i) all(diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) && diff(t$NDZ_sanemsanas_datums[2:3]) == 0) {
                     yt <- y[v, ]
                     yt$dienas <- 0
                   } else {stop("Starpkodi4_51: Trūkst izstrādes koda.")}
                 } else {stop("Starpkodi4_51: Trūkst izstrādes koda.")}
               } else {stop("Starpkodi4_51: Trūkst izstrādes koda.")}
  } else if (t$zinkod[2] %in% c("40", "50", "53", "91")) {
              if (t$zinkod[3] %in% c("21", "22", "23", "24", "25", "29")) {
                if (t$zinkod[4] %in% c("41", "51", "54", "92")) {
                  if (all(diff(t$NDZ_sanemsanas_datums[1:3]) != 0) && diff(t$NDZ_sanemsanas_datums[3:4]) == 0) {
                    yt <- y[v, ]
                    yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[1:2]))
                  } else {stop("Starpkodi4_51: Trūkst izstrādes koda.")}
                } else {stop("Starpkodi4_51: Trūkst izstrādes koda.")}
              } else {stop("Starpkodi4_51: Trūkst izstrādes koda.")}
  } else {stop("Starpkodi4_51: Trūkst izstrādes koda.")}

  rm(y, t, prev, v)
  return(yt) 
}


              
  

#
#  starpkodi4_41 <- function(y, t, prev, v) {
#    
#    if (t$zinkod[2] == "50") {
#      if(t$zinkod[3] == "51") {
#        if (t$zinkod[4] %in% c("40", "50")) {
#          if (diff(t$NDZ_sanemsanas_datums[1:2]) == 0 && all(diff(t$NDZ_sanemsanas_datums[2:4]) != 0)) {
#            yt <- y[v, ]
#            yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[3:4]))
#          } else {
#            yt <- y[v, ]
#            yt$dienas <- sum(sapply(seq(1,4,by=2), function(i) as.numeric(diff(t$NDZ_sanemsanas_datums[i:(i+1)]))))}
#        } else {stop("Starpkodi4_41 iztrūkst apstrādes koda.")} 
#      } else {stop("Starpkodi4_41 iztrūkst apstrādes koda.")}
#    } else 
#      
#      
#    rm(y, t, prev, v)
#    return(yt)
#  }

 # if (t$zinkod[2] == "50" && t$zinkod[3] == "25" && t$zinkod[4] == "51" && 
 #              # } else if (t$zinkod[2] == "50" && t$zinkod[3] == "51" && t$zinkod[4] == "21" && 
 #            diff(t$NDZ_sanemsanas_datums[1:2]) == 0 && diff(t$NDZ_sanemsanas_datums[3:4]) != 0) {
 #   yt <- y[v, ]
 #   yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[4], t$NDZ_sanemsanas_datums[3], units = "days")) + 1 
 # } else if (t$zinkod[2] == "50" && t$zinkod[3] == "21" && t$zinkod[4] == "51" && 
 #            diff(t$NDZ_sanemsanas_datums[1:2]) != 0 && diff(t$NDZ_sanemsanas_datums[3:4]) == 0) {
 #   yt <- y[v, ]
 #   yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[2], t$NDZ_sanemsanas_datums[1], units = "days"))
 # } else if (t$zinkod[2] == "50" && t$zinkod[3] == "51" && t$zinkod[4] == "25" && 
 #            all(diff(t$NDZ_sanemsanas_datums) != 0)) {
 #   yt <- y[v, ]
 #   yt$dienas <- sum(sapply(seq(1,4,by=2), function(i) as.numeric(difftime(t$NDZ_sanemsanas_datums[i+1], t$NDZ_sanemsanas_datums[i], units = "days")))) + 1
 # } else if (t$zinkod[2] == "50" && t$zinkod[3] %in% c("21", "25") && t$zinkod[4] == "51" && 
 #            diff(t$NDZ_sanemsanas_datums[1:2]) == 0 && diff(t$NDZ_sanemsanas_datums[3:4]) == 0 && diff(t$NDZ_sanemsanas_datums[2:3]) != 0) {
 #   yt <- y[v, ]
 #   yt$dienas <- 0
 # } else if (t$zinkod[2] == "91" && t$zinkod[3] == "92" && t$zinkod[4] == "50" && all(diff(t$NDZ_sanemsanas_datums) != 0)) {
 #   days <- 0
 #   for(d in seq(1, 4, by = 2)) {days <- days + as.numeric(difftime(t$NDZ_sanemsanas_datums[d + 1], t$NDZ_sanemsanas_datums[d], units = "days"))} 
 #   
 #   yt <- y[v, ]
 #   yt$dienas <- days
 # } else if (t$zinkod[2] == "91" && t$zinkod[3] == "92" && t$zinkod[4] == "50" && 
 #            all(diff(t$NDZ_sanemsanas_datums[1:3]) != 0) && diff(t$NDZ_sanemsanas_datums[3:4]) == 0) {
 #   yt <- y[v, ]
 #   yt$dienas <- sum(sapply(seq(1,4,by = 2), function(i) as.numeric(difftime(t$NDZ_sanemsanas_datums[i + 1], t$NDZ_sanemsanas_datums[i], units = "days"))))
 # } else if (t$zinkod[2] == "50" && t$zinkod[3] == "51" && t$zinkod[4] == "25" && 
 #            diff(t$NDZ_sanemsanas_datums[1:2]) == 0 && all(diff(t$NDZ_sanemsanas_datums[2:4]) != 0)) {
 #   yt <- y[v, ]
 #   yt$dienas <- sum(sapply(seq(1,4,by = 2), function(i) as.numeric(difftime(t$NDZ_sanemsanas_datums[i + 1], t$NDZ_sanemsanas_datums[i], units = "days")))) + 1
  

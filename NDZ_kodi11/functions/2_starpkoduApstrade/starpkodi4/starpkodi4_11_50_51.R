starpkodi4_11_50_51 <- function(y, t, prev, v) {
  
  if (t$zinkod[4] %in% c("40", "50", "53", "91")){
             if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
               yt <- y[v, ]
               yt$dienas <- sum(as.numeric(diff(t$NDZ_sanemsanas_datums[1:2])), 
                                as.numeric(diff(t$NDZ_sanemsanas_datums[3:4])))
          #   } else if (all(diff(t$NDZ_sanemsanas_datums[2:4]) != 0) && all(diff(t$NDZ_sanemsanas_datums[1:2]) == 0)) {
          #     days1 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[2], t$NDZ_sanemsanas_datums[1], units = "days")) + 1
          #     days2 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[4], t$NDZ_sanemsanas_datums[3], units = "days"))
          #     
          #     yt <- y[v, ]
          #     yt$dienas <- sum(days1, days2)
          #     rm(days1, days2)
             } else {stop("Starpkodi4_11_50_51: Trūkst izstrādes koda.")}
  } else if (t$zinkod[4] %in% c("41", "51", "54", "92")) {
           if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
             yt <- y[v, ]
             yt$dienas <- sum(as.numeric(diff(t$NDZ_sanemsanas_datums[1:2])), 
                              as.numeric(difftime(t$last_date[4], t$NDZ_sanemsanas_datums[4], units = "days")) + 1)
        #  } else if (diff(t$NDZ_sanemsanas_datums[2:3]) == 0 && all(sapply(c(1,3), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
        #      yt <- y[v, ]
        #      yt$dienas <- sum(as.numeric(diff(t$NDZ_sanemsanas_datums[1:2])),
        #                       as.numeric(difftime(t$last_date[4], t$NDZ_sanemsanas_datums[4], units = "days")) + 1)
        #     
         #  } else if (all(diff(t$NDZ_sanemsanas_datums[1:3]) != 0) && diff(t$NDZ_sanemsanas_datums[3:4]) == 0) {
         #        yt <- y[v, ]
         #        yt$dienas <- sum(as.numeric(diff(t$NDZ_sanemsanas_datums[1:2])),
         #                         as.numeric(difftime(t$last_date[4], t$NDZ_sanemsanas_datums[4], units = "days")) + 1)
             } else {stop("Starpkodi4_11_50_51: Trūkst izstrādes koda.")}
  } else {stop("Starpkodi4_11_50_51: Trūkst izstrādes koda.")}
  
     
  

    
#  if(diff(t$NDZ_sanemsanas_datums[1:2]) == 0 && t$zinkod[4] == "21" && diff(t$NDZ_sanemsanas_datums[3:4]) != 0) {
#    yt <- y[v, ]
#    yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[4], t$NDZ_sanemsanas_datums[3], units = "days"))
#  } else if (t$zinkod[4] == "25" && all(t$NDZ_sanemsanas_datums[3:4] != 0)) {
#    yt <- y[v, ]
#    yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[4], t$NDZ_sanemsanas_datums[3], units = "days"))
#  } else if (t$zinkod[4] == "25" && all(diff(t$NDZ_sanemsanas_datums) != 0)) {
#    days1 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[2], t$NDZ_sanemsanas_datums[1], units = "days")) 
#    days2 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[4], t$NDZ_sanemsanas_datums[3], units = "days")) 
#    
#    yt <- y[v, ]
#    yt$dienas <- sum(days1, days2)
#    rm(days1, days2)
#  } else if (t$zinkod[4] == "21" && all(!diff(t$NDZ_sanemsanas_datums) == 0)) {
#    days1 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[2], t$NDZ_sanemsanas_datums[1], units = "days"))
#    days2 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[4], t$NDZ_sanemsanas_datums[3], units = "days"))
#    
#    yt <- y[v, ]
#    yt$dienas <- sum(days1, days2)
#    rm(days1, days2)
#  } else if (t$zinkod[4] == "21" && 
#             all(diff(t$NDZ_sanemsanas_datums[2:3]) == 0) && t$NDZ_sanemsanas_datums[1] != t$NDZ_sanemsanas_datums[2] &&
#             t$NDZ_sanemsanas_datums[3] != t$NDZ_sanemsanas_datums[4]) {
#    yt <- y[v, ]
#    yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[4], t$NDZ_sanemsanas_datums[1], units = "days"))
#  
#  } else if (t$zinkod[4] == "21" && all(diff(t$NDZ_sanemsanas_datums) != 0)) {
#    #Indivīdu pieņem darbā, tas aiziet bezalgas atvaļinājumā, atgriežas no tā un aiziet no darba.
#    days1 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[2], t$NDZ_sanemsanas_datums[1], units = "days"))
#    days2 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[4], t$NDZ_sanemsanas_datums[3], units = "days")) + 1
#    
#    yt <- y[v, ]
#    yt$dienas <- sum(days1, days2)
#    rm(days1, days2)
  
  rm(y, t, prev, v)
  return(yt) 
}

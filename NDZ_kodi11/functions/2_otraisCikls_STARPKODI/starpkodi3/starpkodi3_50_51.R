starpkodi3_50_51 <- function(y, t, prev, v) {

  yt <- y[v, ]
  
    if (t$zinkod[3] %in% c("21", "22", "23", "24", "25", "29")) {
               if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
                 yt$dd <- as.numeric(sum(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days") - 1,  
                                             diff(t$NDZ_sanemsanas_datums[2:3])))
               } else if (diff(t$NDZ_sanemsanas_datums[1:2]) == 0 && diff(t$NDZ_sanemsanas_datums[2:3]) != 0) {
                if (t$period[1] == '______' && t$PS_code[1] %in% c('________', 
                                                                      '________', 
                                                                      '________',
                                                                      '________',
                                                                      '________',
                                                                      '________') && 
                    t$NM_code[1] %in% c('________', 
                                       '________', 
                                       '________',
                                       '________',
                                       '________',
                                       '________')) {
                   yt$dd <- 0
                } else if (t$PS_code[1] %in% c('________', '________', '________', '________') && t$NM_code[1] == '________') {
                  yt$dd <- 0
                } else if (t$period[1] == '' && t$PS_code[1] %in% c('________', '________') && t$NM_code[1] %in% c('________', '________')) {
                  yt$dd <- as.numeric(diff(t$NDZ_sanemsanas_datums[1:2]))
                } else if (t$period[1] == '' && t$PS_code[1] == '________' && t$NM_code[1] == '________') {
                  yt$dd <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1,
                                   as.numeric(diff(t$NDZ_sanemsanas_datums[2:3]))) + 1
                } else {stop("TE IR LABI, KA BREMZĒJAS. \ starpkodi3_50_51: Iztrūkst apstrādes koda.")}
             } else {stop("starpkodi3_50_51: Iztrūkst apstrādes koda.")}
    } else if (t$zinkod[3] %in% c("40", "50", "53", "91")) {
             if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
               yt$dd <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1,  
                                as.numeric(diff(t$NDZ_sanemsanas_datums[2:3])))
             } else if (diff(t$NDZ_sanemsanas_datums[1:2]) != 0 && diff(t$NDZ_sanemsanas_datums[2:3]) == 0) {
               yt$dd <- as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1
             } else if (all(diff(t$NDZ_sanemsanas_datums) == 0)) {
               if(t$period[1] == "________" && t$PS_code[1] == '________' && t$NM_code[1] == '________') {
                 yt$dd <- sum(as.numeric(diff(t$NDZ_sanemsanas_datums[1:2])), as.numeric(difftime(t$last_date[3], t$NDZ_sanemsanas_datums[3], units = "days")) + 1)
               } else {stop("starpkodi3_50_51: Iztrūkst apstrādes koda.")}
             } else {stop("starpkodi3_50_51: Iztrūkst apstrādes koda.")}
  } else if (t$zinkod[3] %in% c("41", "51", "54", "92")) {
             if (diff(t$NDZ_sanemsanas_datums[2:3]) != 0 && diff(t$NDZ_sanemsanas_datums[1:2]) == 0) {
               if ((t$period[1] == "________" && t$PS_code[1] == '________' && t$NM_code[1] == '________') ||
                   (t$period[1] == "______" && t$PS_code[1] == '________' && t$NM_code[1] == '________')) {
                 yt$dd <- as.numeric(sum(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days") - 1,
                                             difftime(t$last_date[3], t$NDZ_sanemsanas_datums[3], units = "days") + 1))
               } else if ((t$period[1] == "______" && t$PS_code[1] == '________' && t$NM_code[1] == '________') ||
                          (t$period[1] == "________" && t$PS_code[1] == '________' && t$NM_code[1] == '________')) {
                          yt$dd <- sum(as.numeric(diff(t$NDZ_sanemsanas_datums[1:2])),
                                        as.numeric(difftime(t$last_date[3], t$NDZ_sanemsanas_datums[3], units = "days")) + 1)
               } else {stop("starpkodi3_50_51: Iztrūkst apstrādes koda.")}
             } else {stop("starpkodi3_50_51: Iztrūkst apstrādes koda.")}     
  } else {stop("starpkodi3_50_51: Iztrūkst apstrādes koda.")}
  
rm(y, t, prev, v)
return(yt) 
}






#} else 
#} else if (t$zinkod[3] == "53" && all(diff(t$NDZ_sanemsanas_datums) != 0)) {
# days1 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1
#  days2 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[3], t$NDZ_sanemsanas_datums[2], units = "days"))

# yt <- y[v, ]
#  yt$dd <- sum(days1, days2)
# rm(days1, days2)
#} else if (t$zinkod[3] == "25" && all(diff(t$NDZ_sanemsanas_datums) != 0)) {
#  days1 <- as.numeric(difftime(t$beidz_darbu[1], prev, units = "days")) - 1
#  days2 <- as.numeric(difftime(t$beidz_darbu[3], t$sak_darbu[2], units = "days")) + 1
  
#  yt <- y2[v, ]
#  yt$dd <- sum(days1, days2)
#  rm(days1, days2)
#} else {
#  stop("Starpkodi3_40: Trūkst izstrādes koda.")
#}

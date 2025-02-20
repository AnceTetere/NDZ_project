starpkodi3_50_51 <- function(y, t, prev, v) {

  yt <- y[v, ]
  
    if (t$zinkod[3] %in% c("21", "22", "23", "24", "25", "29")) {
               if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
                 yt$dd <- as.numeric(sum(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "dd") - 1, 
                                             diff(t$NDZ_sanemsanas_datums[2:3])))
               } else if (diff(t$NDZ_sanemsanas_datums[1:2]) == 0 && diff(t$NDZ_sanemsanas_datums[2:3]) != 0) {
                if (t$period[1] == '______' && t$PS_code[1] %in% c('______', 
                                                                      '______', 
                                                                      '______',
                                                                      '______',
                                                                      '______',
                                                                      '______') && 
                    t$NM_code[1] %in% c('______', 
                                        '______', 
                                        '______',
                                        '______',
                                        '______',
                                        '______')) {
                   yt$dd <- 0
                } else if (t$PS_code[1] %in% c('______', '______', '______', '______') && t$NM_code[1] == '______') {
                  yt$dd <- 0
                } else if (t$period[1] == '______' && t$PS_code[1] %in% c('______', '______') && t$NM_code[1] %in% c('______', '______')) {
                  yt$dd <- as.numeric(diff(t$NDZ_sanemsanas_datums[1:2]))
                } else if (t$period[1] == '______' && t$PS_code[1] == '______' && t$NM_code[1] == '______') {
                  yt$dd <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "dd")) - 1,
                                   as.numeric(diff(t$NDZ_sanemsanas_datums[2:3]))) + 1
                } else {stop("TE IR LABI, KA BREMZĒJAS. \ starpkodi3_50_51: Iztrūkst apstrādes koda.")}
             } else {stop("starpkodi3_50_51: Iztrūkst apstrādes koda.")}
    } else if (t$zinkod[3] %in% c("40", "50", "53", "91")) {
             if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
               yt$dd <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "dd")) - 1, 
                                as.numeric(diff(t$NDZ_sanemsanas_datums[2:3])))
             } else if (diff(t$NDZ_sanemsanas_datums[1:2]) != 0 && diff(t$NDZ_sanemsanas_datums[2:3]) == 0) {
               yt$dd <- as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "dd")) - 1
             } else if (all(diff(t$NDZ_sanemsanas_datums) == 0)) {
               if(t$period[1] == "______" && t$PS_code[1] == '______' && t$NM_code[1] == '______') {
                 yt$dd <- sum(as.numeric(diff(t$NDZ_sanemsanas_datums[1:2])), as.numeric(difftime(t$last_date[3], t$NDZ_sanemsanas_datums[3], units = "dd")) + 1)
               } else {stop("starpkodi3_50_51: Iztrūkst apstrādes koda.")}
             } else {stop("starpkodi3_50_51: Iztrūkst apstrādes koda.")}
  } else if (t$zinkod[3] %in% c("41", "51", "54", "92")) {
             if (diff(t$NDZ_sanemsanas_datums[2:3]) != 0 && diff(t$NDZ_sanemsanas_datums[1:2]) == 0) {
               if ((t$period[1] == "______" && t$PS_code[1] == '______' && t$NM_code[1] == '______') ||
                   (t$period[1] == "_____" && t$PS_code[1] == '_____' && t$NM_code[1] == '_____')) {
                 yt$dd <- as.numeric(sum(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "dd") - 1,
                                             difftime(t$last_date[3], t$NDZ_sanemsanas_datums[3], units = "dd") + 1))
               } else if ((t$period[1] == "_____" && t$PS_code[1] == '_____' && t$NM_code[1] == '_____') ||
                          (t$period[1] == "______" && t$PS_code[1] == '_____' && t$NM_code[1] == '_____') ||
                          (t$period[1] == "______" && t$PS_code[1] == '_____' && t$NM_code[1] == '_____') ||
                          (t$period[1] == "______" && t$PS_code[1] == '_____' && t$NM_code[1] == '_____')) {
                          yt$dd <- sum(as.numeric(diff(t$NDZ_sanemsanas_datums[1:2])),
                                        as.numeric(difftime(t$last_date[3], t$NDZ_sanemsanas_datums[3], units = "dd")) + 1)
               } else {stop("starpkodi3_50_51: Iztrūkst apstrādes koda.")}
             } else {stop("starpkodi3_50_51: Iztrūkst apstrādes koda.")}     
  } else {stop("starpkodi3_50_51: Iztrūkst apstrādes koda.")}
  
rm(y, t, prev, v)
return(yt) 
}



#} else 
#} else if (t$zinkod[3] == "53" && all(diff(t$NDZ_sanemsanas_datums) != 0)) {
# dd1 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "dd")) - 1
#  dd2 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[3], t$NDZ_sanemsanas_datums[2], units = "dd"))

# yt <- y[v, ]
#  yt$dd <- sum(dd1, dd2)
# rm(dd1, dd2)
#} else if (t$zinkod[3] == "25" && all(diff(t$NDZ_sanemsanas_datums) != 0)) {
#  dd1 <- as.numeric(difftime(t$beidz_darbu[1], prev, units = "dd")) - 1
#  dd2 <- as.numeric(difftime(t$beidz_darbu[3], t$sak_darbu[2], units = "dd")) + 1
  
#  yt <- y2[v, ]
#  yt$dd <- sum(dd1, dd2)
#  rm(dd1, dd2)
#} else {
#  stop("Starpkodi3_40: Trūkst izstrādes koda.")
#}

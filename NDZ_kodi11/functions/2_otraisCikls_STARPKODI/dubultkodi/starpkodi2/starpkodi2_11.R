starpkodi2_11 <- function(y, t, prev, v) {

  yt <- y[v,]
  
  if (t$zinkod[2] %in% c("50", "53", "40", "91")) {
      yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[1:2]))
      ZERO_plus(t %>% slice(2))
  } else if (t$zinkod[2] %in% c("41", "51", "54", "92")) {
           if (diff(t$NDZ_sanemsanas_datums) != 0) {
              if ((t$period[1] == '______' && t$PS_code[1] ==  '______________' && t$NM_code[1] ==  '______________') ||
                  (t$period[1] == '______' && t$PS_code[1] ==  '______________' && t$NM_code[1] ==  '______________') ||
                  (t$period[1] == '______' && t$PS_code[1] ==  '______________' && t$NM_code[1] ==  '______________') ||
                  (t$period[1] == '______' && t$PS_code[1] ==  '______________' && t$NM_code[1] ==  '______________') ||
                  (t$period[1] == '______' && t$PS_code[1] ==  '______________' && t$NM_code[1] ==  '______________')) {
                   yt$dienas <- as.numeric(difftime(t$last_date[2], t$NDZ_sanemsanas_datums[1], units = "days")) + 1
              } else {stop("Starpkodi2_11: Trūkst izstrādes koda.")}
           } else {stop("Starpkodi2_11: Trūkst izstrādes koda.")}
  } else {stop("Starpkodi2_11: Trūkst izstrādes koda.")}
  
  rm(y, t, prev, v)
  return(yt)
}

#  } else if (t$zinkod[2] %in% c("41", "51", "54", "92")) {
#if (t$PS_code[1] %in% c('_______', '_______', '_______', '_______', '_______', '_______', '_______') && t$NM_code[1] %in% c('_______')) {
#  yt$dienas <- as.numeric(difftime(t$last_date[2], t$NDZ_sanemsanas_datums[2], units = "days")) + 1
#} else if (t$zinkod[2] == "51" && t$PS_code[1] %in% c('_______', '_______', '_______', '_______', '_______', '_______', '_______', '_______', '_______') && t$NM_code[1] %in% c('_______', '_______', '_______')) {
#  yt <- y[v:(v+1),] 
#  yt <- yt[yt$zinkod == "11", ]
#} else if (t$zinkod[2] == "41" && t$PS_code[1] ==  '______________' && t$NM_code[1] ==  '______________') {
#  yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[1:2])) 
#} else if (t$zinkod[2] == "51" && t$PS_code[1] %in% c('_______') && t$NM_code[1] %in% c('_______')) {
#  yt$dienas <- as.numeric(difftime(t$last_date[2], t$NDZ_sanemsanas_datums[2], units = "days"))
#} else if (t$zinkod[2] == "51" && t$PS_code[1] %in% c('_______') && t$NM_code[1] %in% c('_______')) {
#  yt$dienas <- as.numeric(difftime(t$last_date[2], t$NDZ_sanemsanas_datums[2], units = "days"))
#} else if (t$zinkod[2] == "41" && t$PS_code[1] ==  '______________' && t$NM_code[1] ==  '______________' && t$period[1] == '______') {
#  #Te izskatās pēc kļūdas un tas 11 kods ir nesaprotams, kamēr "41" vietā būtu jābūt "92" 
#  yt$dienas <- as.numeric(difftime(t$last_date[2], t$NDZ_sanemsanas_datums[2], units = "days")) + 1
#} else if (t$zinkod[2] == "51" && t$PS_code[1] ==  '______________' && t$NM_code[1] ==  '______________' && t$period[1] == '______') {
#  yt$dienas <- as.numeric(difftime(t$last_date[2], t$NDZ_sanemsanas_datums[1], units = "days")) + 1
#} else {stop("Starpkodi2_11: Trūkst izstrādes koda.")}

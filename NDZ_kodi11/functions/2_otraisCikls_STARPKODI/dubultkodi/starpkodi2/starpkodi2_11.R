starpkodi2_11 <- function(y, t, prev, v) {

  yt <- y[v,]
  
  if (t$zinkod[2] %in% c("50", "53", "40", "91")) {
      yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[1:2]))
      ZERO_plus(t %>% slice(2))
  } else if (t$zinkod[2] %in% c("41", "51", "54", "92")) {
           if (diff(t$NDZ_sanemsanas_datums) != 0) {
              if ((t$period[1] == '______' && tPS_code ==  '______________' && tNM_code ==  '______________') ||
                  (t$period[1] == '______' && tPS_code ==  '______________' && tNM_code ==  '______________') ||
                  (t$period[1] == '______' && tPS_code ==  '______________' && tNM_code ==  '______________') ||
                  (t$period[1] == '______' && tPS_code ==  '______________' && tNM_code ==  '______________') ||
                  (t$period[1] == '______' && tPS_code ==  '______________' && tNM_code ==  '______________')) {
                   yt$dienas <- as.numeric(difftime(t$last_date[2], t$NDZ_sanemsanas_datums[1], units = "days")) + 1
              } else {stop("Starpkodi2_11: Trūkst izstrādes koda.")}
           } else {stop("Starpkodi2_11: Trūkst izstrādes koda.")}
  } else {stop("Starpkodi2_11: Trūkst izstrādes koda.")}
  
  rm(y, t, prev, v)
  return(yt)
}

#  } else if (t$zinkod[2] %in% c("41", "51", "54", "92")) {
#if (t$PS_code[1] %in% c('______', '______', '______', '______', '______', '______', '______') && t$NM_code[1] %in% c('______')) {
#  yt$dienas <- as.numeric(difftime(t$last_date[2], t$NDZ_sanemsanas_datums[2], units = "days")) + 1
#} else if (t$zinkod[2] == "51" && t$PS_code[1] %in% c('______', '______', '______', '______', '______', '______', '______', '______', '______') && t$NM_code[1] %in% c('______', '______', '______')) {
#  yt <- y[v:(v+1),] 
#  yt <- yt[yt$zinkod == "11", ]
#} else if (t$zinkod[2] == "41" && tPS_code ==  '______________' && tNM_code ==  '______________') {
#  yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[1:2])) 
#} else if (t$zinkod[2] == "51" && t$PS_code[1] %in% c('PKBC5FCBC03') && t$NM_code[1] %in% c('40203196271')) {
#  yt$dienas <- as.numeric(difftime(t$last_date[2], t$NDZ_sanemsanas_datums[2], units = "days"))
#} else if (t$zinkod[2] == "51" && t$PS_code[1] %in% c('PK27D6A6F37') && t$NM_code[1] %in% c('40003424678')) {
#  yt$dienas <- as.numeric(difftime(t$last_date[2], t$NDZ_sanemsanas_datums[2], units = "days"))
#} else if (t$zinkod[2] == "41" && tPS_code ==  '______________' && tNM_code ==  '______________' && t$period[1] == '______') {
#  yt$dienas <- as.numeric(difftime(t$last_date[2], t$NDZ_sanemsanas_datums[2], units = "days")) + 1
#} else if (t$zinkod[2] == "51" && tPS_code ==  '______________' && tNM_code ==  '______________' && t$period[1] == '______') {
#  yt$dienas <- as.numeric(difftime(t$last_date[2], t$NDZ_sanemsanas_datums[1], units = "days")) + 1
#} else {stop("Starpkodi2_11: Trūkst izstrādes koda.")}


starpkodi2_11 <- function(y, t, prev, v) {

  yt <- y[v,]
  
  if (t$zinkod[2] %in% c("50", "53", "40", "91")) {
      yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[1:2]))
      ZERO_plus(t %>% slice(2))
  } else if (t$zinkod[2] %in% c("41", "51", "54", "92")) {
    if (t$PS_code[1] %in% c('__________', '__________', '__________', '__________', '__________', '__________', '__________') && t$NM_code[1] %in% c('__________')) {
      yt$dienas <- as.numeric(difftime(t$last_date[2], t$NDZ_sanemsanas_datums[2], units = "days")) + 1
    } else if (t$zinkod[2] == "51" && t$PS_code[1] %in% c('__________', '__________', '__________', '__________', '__________', '__________', '__________', '__________', '__________') && t$NM_code[1] %in% c('__________', '__________', '__________')) {
      yt <- y[v:(v+1),] 
      yt <- yt[yt$zinkod == "11", ]
    } else if (t$zinkod[2] == "41" && t$PS_code[1] == '__________' && t$NM_code[1] == '__________') {
      yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[1:2])) 
    } else if (t$zinkod[2] == "51" && t$PS_code[1] %in% c('__________') && t$NM_code[1] %in% c('__________')) {
      yt$dienas <- as.numeric(difftime(t$last_date[2], t$NDZ_sanemsanas_datums[2], units = "days"))
    } else if (t$zinkod[2] == "51" && t$PS_code[1] %in% c('__________') && t$NM_code[1] %in% c('__________')) {
      yt$dienas <- as.numeric(difftime(t$last_date[2], t$NDZ_sanemsanas_datums[2], units = "days"))
    } else if (t$zinkod[2] == "41" && t$PS_code[1] == '__________' && t$NM_code[1] == '__________' && t$period[1] == '_____') {
      yt$dienas <- as.numeric(difftime(t$last_date[2], t$NDZ_sanemsanas_datums[2], units = "days")) + 1
    } else if (t$zinkod[2] == "51" && t$PS_code[1] == '__________' && t$NM_code[1] == '__________' && t$period[1] == '_____') {
      yt$dienas <- as.numeric(difftime(t$last_date[2], t$NDZ_sanemsanas_datums[1], units = "days")) + 1
    } else {stop("Starpkodi2_11: Trūkst izstrādes koda.")}

    
    
  #if (t$zinkod[2] == "41" && t$PS_code[1] == 'PK8DE2E0986' && t$NM_code[1] == '__________') {
   # yt <- y[v,] 
  #  yt$dienas <- as.numeric(difftime(t$last_date[2], t$sak_darbu[2], units = "days")) + 1 
  #} else if (t$zinkod[2] == "51" && t$PS_code[1] == '__________' && t$NM_code[1] == '__________') {
   # yt <- y[v:(v+1),] 
    #yt <- yt[yt$zinkod == "11", ] 
  #} else if (t$zinkod[2] == "91") {
#    yt <- y[v,] 
#    yt$dienas <- as.numeric(difftime(t$beidz_darbu[2], t$sak_darbu[1], units = "days"))
#  } else if (t$zinkod[2] == "53") {
#    yt <- y[v,] 
#    yt$dienas <- as.numeric(difftime(t$beidz_darbu[2], t$sak_darbu[1], units = "days"))
  } else {stop("Starpkodi2_11: Trūkst izstrādes koda.")}
  
  rm(y, t, prev, v)
  return(yt)
}


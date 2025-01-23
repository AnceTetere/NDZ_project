starpkodi2_11 <- function(y, t, prev, v) {

  yt <- y[v,]
  
  if (t$zinkod[2] %in% c("50", "53", "40", "91")) {
      yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[1:2]))
  } else if (t$zinkod[2] %in% c("41", "51", "54", "92")) {
    if (t$PS_code[1] %in% c('_________', '_________', '_________', '_________', '_________', '_________', '_________') && t$NM_code[1] %in% c('_________')) {
      yt$dienas <- as.numeric(difftime(t$last_date[2], t$NDZ_sanemsanas_datums[2], units = "days")) + 1
    } else if (t$zinkod[2] == "51" && t$PS_code[1] %in% c('_________', '_________', '_________', '_________', '_________', '_________', '_________', '_________', '_________') && t$NM_code[1] %in% c('_________', '_________', '_________')) {
      yt <- y[v:(v+1),] 
      yt <- yt[yt$zinkod == "11", ]
    } else if (t$zinkod[2] == "41" && t$PS_code[1] == '_________' && t$NM_code[1] == '_________') {
      yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[1:2])) 
    } else if (t$zinkod[2] == "51" && t$PS_code[1] %in% c('_________') && t$NM_code[1] %in% c('_________')) {
      yt$dienas <- as.numeric(difftime(t$last_date[2], t$NDZ_sanemsanas_datums[2], units = "days"))
    } else if (t$zinkod[2] == "51" && t$PS_code[1] %in% c('_________') && t$NM_code[1] %in% c('_________')) {
      yt$dienas <- as.numeric(difftime(t$last_date[2], t$NDZ_sanemsanas_datums[2], units = "days"))
    } else {stop("Starpkodi2_11: Trūkst izstrādes koda.")}

    
    
    
    
  #if (t$zinkod[2] == "41" && t$PS_code[1] == '_________' && t$NM_code[1] == '_________') {
   # yt <- y[v,] 
  #  yt$dienas <- as.numeric(difftime(t$last_date[2], t$sak_darbu[2], units = "days")) + 1 
  #} else if (t$zinkod[2] == "51" && t$PS_code[1] == '_________' && t$NM_code[1] == '_________') {
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

starpkodi2_11 <- function(y, t, prev, v) {

  if (t$zinkod[2] %in% c("50", "53", "40", "91")) {
      yt <- y[v,]
      yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[1:2]))
  } else if (t$zinkod[2] %in% c("41", "51", "54", "92")) {
    if (t$PS_code[1] %in% c('____________', '____________', '____________', '____________', '____________', '____________', '____________') && t$NM_code[1] %in% c('____________')) {
      yt <- y[v,] 
      yt$dienas <- as.numeric(difftime(t$last_date[2], t$NDZ_sanemsanas_datums[2], units = "days")) + 1
    } else if (t$zinkod[2] == "51" && t$PS_code[1] %in% c('____________', '____________', '____________', '____________', '____________', '____________', '____________', '____________') && t$NM_code[1] %in% c('____________', '____________')) {
      yt <- y[v:(v+1),] 
      yt <- yt[yt$zinkod == "11", ]
    } else {stop("Starpkodi2_11: Trūkst izstrādes koda.")}

    
    
    
    
  #  } else if (diff(t$NDZ_sanemsanas_datums) != 0 && t$PS_code[1] == '____________' && t$NM_code[1] == '____________') {
  #      yt <- y[v:(v+1),] 
  #      yt <- yt[yt$zinkod == "50", ]
  #    } else {stop("Starpkodi2_11: Trūkst izstrādes koda.")}
  #if (t$zinkod[2] == "41" && t$PS_code[1] == '____________' && t$NM_code[1] == '____________') {
   # yt <- y[v,] 
  #  yt$dienas <- as.numeric(difftime(t$last_date[2], t$sak[2], units = "days")) + 1 
  #} else if (t$zinkod[2] == "51" && t$PS_code[1] == '____________' && t$NM_code[1] == '____________') {
   # yt <- y[v:(v+1),] 
    #yt <- yt[yt$zinkod == "11", ] 
  #} else if (t$zinkod[2] == "91") {
#    yt <- y[v,] 
#    yt$dienas <- as.numeric(difftime(t$beidz[2], t$sak[1], units = "days"))
#  } else if (t$zinkod[2] == "53") {
#    yt <- y[v,] 
#    yt$dienas <- as.numeric(difftime(t$beidz[2], t$sak[1], units = "days"))
#  } else if (t$zinkod[2] == "41" && t$PS_code[1] == '____________' & t$NM_code[1] == '____________') {
#    yt <- y[v,] 
#    yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[2], t$sak[1], units = "days"))
  } else {stop("Starpkodi2_11: Trūkst izstrādes koda.")}
  
  rm(y, t, prev, v)
  return(yt)
}

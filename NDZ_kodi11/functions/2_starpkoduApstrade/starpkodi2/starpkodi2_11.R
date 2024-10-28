starpkodi2_11 <- function(y, t, prev, v) {

  if (t$zinkod[2] == "50") {
      yt <- y[v,]
      yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[1:2]))
  } else if (t$zinkod[2] == "40") {
      yt <- y2[v,] 
      yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[1:2]))
  } else if (t$zinkod[2] == "51") {
    if (t$PS_code[1] == '___________' && t$NM_code[1] == '____________') {
      yt <- y2[v,] 
      yt$dienas <- as.numeric(difftime(t$last_date[2], t$NDZ_sanemsanas_datums[2], units = "days")) + 1
    }
  #} else if (t$zinkod[2] == "51")  {
  #    if(t$PS_code[1] %in% c('___________', '___________') && t$NM_code[1] == '___________') {
  #      yt <- y2[v:(v+1),] 
  #      yt <- yt[yt$zinkod == "11", ]
  #    } else if (diff(t$NDZ_sanemsanas_datums) != 0 && t$PS_code[1] == '___________' && t$NM_code[1] == '___________') {
  #      yt <- y2[v:(v+1),] 
  #      yt <- yt[yt$zinkod == "50", ]
  #    } else {stop("Starpkodi2_11: Trūkst izstrādes koda.")}
  #if (t$zinkod[2] == "41" && t$PS_code[1] == '___________' && t$NM_code[1] == '___________') {
   # yt <- y2[v,] 
  #  yt$dienas <- as.numeric(difftime(t$last_date[2], t$sak_darbu[2], units = "days")) + 1 
  #} else if (t$zinkod[2] == "51" && t$PS_code[1] == '___________' && t$NM_code[1] == '___________') {
   # yt <- y2[v:(v+1),] 
    #yt <- yt[yt$zinkod == "11", ] 
  #} else if (t$zinkod[2] == "91") {
#    yt <- y2[v,] 
#    yt$dienas <- as.numeric(difftime(t$beidz_darbu[2], t$sak_darbu[1], units = "days"))
#  } else if (t$zinkod[2] == "53") {
#    yt <- y2[v,] 
#    yt$dienas <- as.numeric(difftime(t$beidz_darbu[2], t$sak_darbu[1], units = "days"))
#  } else if (t$zinkod[2] == "41" && t$PS_code[1] == 'PKF4BDF5543' & t$NM_code[1] == '90000074812') {
#    yt <- y2[v,] 
#    yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[2], t$sak_darbu[1], units = "days"))
  } else {stop("Starpkodi2_11: Trūkst izstrādes koda.")}
  
  if(is.na(yt$PS_code[1])) {stop("Dienas NA.")}
  yt$zinkod <- "combined"  #jo starpkodu dienu sarēķins
  return(yt)
}

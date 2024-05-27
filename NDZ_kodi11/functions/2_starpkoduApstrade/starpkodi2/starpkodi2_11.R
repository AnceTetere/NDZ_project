starpkodi2_11 <- function(y2, t, prev, v) {
  
  if (t$zinkod[2] == "41" && t$PS_code[1] == '_________' && t$NM_code[1] == '_________') {
    yt <- y2[v,] 
    yt$dienas <- as.numeric(difftime(t$last_date[2], t$sak[2], units = "days")) + 1 
  } else if (t$zinkod[2] == "51" && t$PS_code[1] == '_________' && t$NM_code[1] == '_________') {
    yt <- y2[v:(v+1),] 
    yt <- yt[yt$zinkod == "11", ] 
  } else if (t$zinkod[2] == "51" && t$PS_code[1] %in% c('_________', '_________) && t$NM_code[1] == '_________') {
    yt <- y2[v:(v+1),] 
    yt <- yt[yt$zinkod == "11", ] 
  } else if(t$zinkod[2] == "50") {
    # Sāka darbu un aizgāja bezalgas atvaļinājumā
    yt <- y2[v,]
    yt$dienas <- as.numeric(difftime(t$beidz[2], t$sak[1], units = "days"))
  } else if (t$zinkod[2] == "91") {
    yt <- y2[v,] 
    yt$dienas <- as.numeric(difftime(t$beidz[2], t$sak[1], units = "days"))
  } else if (t$zinkod[2] == "40") {
    yt <- y2[v,] 
    yt$dienas <- as.numeric(difftime(t$beidz[2], t$sak[1], units = "days"))
  } else if (t$zinkod[2] == "53") {
    yt <- y2[v,] 
    yt$dienas <- as.numeric(difftime(t$beidz[2], t$sak[1], units = "days"))
  } else {
    stop("Starpkodi2_11: Trūkst izstrādes koda.")
  }
  
  if(is.na(yt$PS_code[1])) {stop("Dienas NA.")}
  yt$zinkod <- "combined"  #jo starpkodu dienu sarēķins
  return(yt)
}

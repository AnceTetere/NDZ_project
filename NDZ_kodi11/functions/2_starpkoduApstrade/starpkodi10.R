starpkodi10 <- function(y2, t, prev) {
  
  if (t$zinkod[1] == "51" && 
      t$zinkod[2] == "50" && 
      t$zinkod[3] == "51" && 
      t$zinkod[4] == "50" && 
      t$zinkod[5] == "51" && 
      t$zinkod[6] == "50" && 
      t$zinkod[7] == "51" && 
      t$zinkod[8] == "50" && 
      t$zinkod[9] == "21" && 
      t$zinkod[10] == "51" && 
      all(!diff(t$NDZ_sanemsanas_datums[1:9]) == 0) &&
      t$NDZ_sanemsanas_datums[9] == t$NDZ_sanemsanas_datums[10]) {
    #Indivīds atnāk un aiziet daudzos bezalgas atvaļinājumos līdz aiziet pavisam.
    yt <- y2[v:(v+1), ]
    yt <- yt[yt$zinkod == "50", ]
  } else {
    stop("Starpkodi10 iztrūkst apstrādes koda.")
  }
  
  yt$zinkod <- "combined"  #jo starpkodu dienu sarēķins
  return(yt)
  rm(days, yt)
}

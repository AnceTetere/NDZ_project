starpkodi6_11 <- function(y2, t, prev, v) {
  
   if (t$zinkod[2] %in% c("50", "53") && #abi ir beidz kodi 
             t$zinkod[3] %in% c("51", "54") && #abi ir sak kodi
             t$zinkod[4] %in% c("50", "53") && 
             t$zinkod[5] %in% c("51", "54") && 
             t$zinkod[6] %in% c("50", "21") && #abi ir beidz kodi
             all(!diff(t$NDZ_sanemsanas_datums) == 0)) {
    #G1: Indivīds tiek pieņemts darbā un tad vienu pēc otra ņem bezalgas atvaļinājumus.
    #G2: #Indivīds tiek pieņemts darbā un tad iet divās dīkstāvēs un tiek atlaists.
    days1 <- as.numeric(difftime(t$beidz[2], t$sak[1], units = "days"))
    days2 <- as.numeric(difftime(t$beidz[4], t$sak[3], units = "days"))
    days3 <- as.numeric(difftime(t$beidz[6], t$sak[5], units = "days"))
    
    days <- sum(days1, days2, days3)
    rm(days1, days2, days3)
    
    yt <- y2[v, ]
    yt$dienas <- days
  } else if (t$zinkod[2] == "53" && 
             t$zinkod[3] == "54" && 
             t$zinkod[4] == "53" && 
             t$zinkod[5] == "53" && 
             t$zinkod[6] == "54" && 
             all(diff(t$NDZ_sanemsanas_datums[1:2]) == 0) &&
             all(diff(t$NDZ_sanemsanas_datums[5:6]) == 0) &&
             all(!diff(t$NDZ_sanemsanas_datums[2:5]) == 0)) {
    #Indivīds pieņemts darbā un tad iet daudzās dīkstāvēs.
    days1 <- as.numeric(difftime(t$beidz[2], t$sak[1], units = "days")) + 1 #mazliet jau bija jāpastrādā, lai tiktu dīkstāvē paša vainas dēļ.
    days2 <- as.numeric(difftime(t$beidz[4], t$sak[3], units = "days"))
    days3 <- as.numeric(difftime(t$beidz[5], t$sak[6], units = "days"))
    days <- sum(days1, days2, days3)
    rm(days1, days2, days3)
    
    yt <- y2[v, ]
    yt$dienas <- days
  } else if (t$zinkod[2] == "26" && 
             t$zinkod[3] == "11" && 
             t$zinkod[4] == "50" && 
             t$zinkod[5] == "51" && 
             t$zinkod[6] == "25" && 
             all(diff(t$NDZ_sanemsanas_datums[1:2]) == 0) &&
             all(diff(t$NDZ_sanemsanas_datums[2:6]) != 0)) {
    #Indivīdu pieņem darbā un tanī pat dienā atlaiž par darba neuzsākšanu.
    #Tad pieņem vēlāk. Tad tas aiziet bezalgas atvaļinājumā un atnāl no tā, un tad tiek atlaists.
    days1 <- as.numeric(difftime(t$beidz[4], t$sak[3], units = "days"))
    days2 <- as.numeric(difftime(t$beidz[6], t$sak[5], units = "days")) + 1 #jo atlaišana
    
    yt <- y2[v, ]
    yt$dienas <- sum(days1, days2)
    rm(days1, days2)
  } else if (t$zinkod[2] == "50" && 
             t$zinkod[3] == "51" && 
             t$zinkod[4] == "50" && 
             t$zinkod[5] == "25" && 
             t$zinkod[6] == "51" && 
             all(diff(t$NDZ_sanemsanas_datums[5:6]) == 0) &&
             all(diff(t$NDZ_sanemsanas_datums[1:5]) != 0)) {
    #Indivīdu pieņem darbā - tas aiziet bezalgas atvaļinājumā - atnāk no tā un aiziet vēlreiz - dienā, kad jāsāk darbs pēc tā, tiek atlaists.
    #Pieņemu, ka atlaišanas fakts tika paziņots, un cilvēks nestrādāja to dienu.
    days1 <- as.numeric(difftime(t$beidz[2], t$sak[1], units = "days"))
    days2 <- as.numeric(difftime(t$beidz[4], t$sak[3], units = "days"))
    
    yt <- y2[v, ]
    yt$dienas <- sum(days1, days2)
    rm(days1, days2)
  } else if (t$zinkod[2] == "50" && 
             t$zinkod[3] == "51" && 
             t$zinkod[4] == "50" && 
             t$zinkod[5] == "51" && 
             t$zinkod[6] == "25" && 
             all(diff(t$NDZ_sanemsanas_datums) != 0)) {
    #Indivīdu pieņem darbā - tas aiziet bezalgas atvaļinājumā - atnāk no tā un aiziet vēlreiz - tad tiek atlaists.
    #Pieņemu, ka atlaišanas fakts tika paziņots, un cilvēks nestrādāja to dienu.
    days1 <- as.numeric(difftime(t$beidz[2], t$sak[1], units = "days"))
    days2 <- as.numeric(difftime(t$beidz[4], t$sak[3], units = "days"))
    days3 <- as.numeric(difftime(t$beidz[6], t$sak[5], units = "days")) + 1 # jo atlaišana
    
    yt <- y2[v, ]
    yt$dienas <- sum(days1, days2, days3)
    rm(days1, days2, days3)
  } else if (t$zinkod[2] == "50" && t$zinkod[3] == "50" && 
             t$zinkod[4] == "51" && t$zinkod[5] == "21" && 
             t$zinkod[6] == "51" && all(sapply(seq(3, 6, by = 2), function(i) all(diff(t$NDZ_sanemsanas_datums[i+1]) == 0))) &&
             all(diff(t$NDZ_sanemsanas_datums[1:3]) != 0)) {
    #Indivīdu pieņem darbā - tas aiziet bezalgas atvaļinājumā, tam beidzoties, uzreiz ņem nākamo.
    #Otrajam bezalgas atvaļinājumam beidzoties, aiziet no darba pavisam.
    yt <- y2[v, ]
    yt$dienas <- as.numeric(difftime(t$beidz[2], t$sak[1], units = "days"))
  } else {
    stop("starpkodi6_11: Trūkst izstrādes koda starpkodi2.")
  }
  
  if(is.na(yt$PS_code[1])) {stop("Dienas NA.")}
  return(yt)
}


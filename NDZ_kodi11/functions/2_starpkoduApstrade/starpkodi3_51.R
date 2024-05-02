starpkodi3_51 <- function(y2, t, prev, v) {
  if (t$zinkod[2] == "50" && t$zinkod[3] == "25" && all(!diff(t$NDZ_sanemsanas_datums) == 0)) {
    #Indivīds paņem bezalgas atvaļinājumu, atgrižas, pastrādā un aiziet bērni kopšanas atvaļinājumā.
    yt <- y2[v, ]
    yt$dienas <- as.numeric(difftime(as.Date(t$beidz[2]), as.Date(t$sak[1]), units = "days"))
  } else if (t$zinkod[2] == "50" && t$zinkod[3] == "21" && all(!diff(t$NDZ_sanemsanas_datums) == 0)) {
    #Indivīds sāk darbu pēc bezalgas atvaļinājuma, aiziet atvaļinājumā vēlreiz un drīz aiziet pavisam.
    yt <- y2[v, ]
    yt$dienas <- as.numeric(difftime(as.Date(t$beidz[2]), as.Date(t$sak[1]), units = "days"))
  } else if (t$zinkod[2] == "50" && t$zinkod[3] == "25" && all(!diff(t$NDZ_sanemsanas_datums[2:3]) == 0) &&
             t$NDZ_sanemsanas_datums[1] == t$NDZ_sanemsanas_datums[2]) {
    #Indivīds paņem bezalgas atvaļinājumu uz dienu, tad pastrādā un tiek atlaists
    days1 <- as.numeric(difftime(t$beidz[2], prev, units = "days")) - 1 #jo atvaļinājums
    days2 <- as.numeric(difftime(t$beidz[3], t$sak[1], units = "days"))
    
    yt <- y2[v, ]
    yt$dienas <- sum(days1, days2)
    rm(days1, days2)
  } else if (t$zinkod[2] == "21" && t$zinkod[3] == "11" && all(!diff(t$NDZ_sanemsanas_datums) == 0)) {
    #Indivīds sācis darbu pēc bezalgas atvālinājuma, tad aizgājis no darba pēc paša vēlēšanās.
    #Un sācis darbu uzņēmumā no jauna.
    days1 <- as.numeric(difftime(t$beidz[2], t$sak[1], units = "days")) + 1 #jo darbs 
    days2 <- as.numeric(difftime(t$last_date[3], t$sak[3], units = "days"))  + 1 #jo darbs
    
    yt <- y2[v, ]
    yt$dienas <- sum(days1, days2)
    rm(days1, days2)
  } else if (t$zinkod[2] == "25" && t$zinkod[3] == "50" && 
             all(!diff(t$NDZ_sanemsanas_datums[1:2]) == 0) && t$NDZ_sanemsanas_datums[2] == t$NDZ_sanemsanas_datums[3]) {
    #Indivīds atsācis darbu pēc bezalgas atvālinājuma, bet līdz ar nākamā bezalgas atvaļinājuma sākumu
    #tiek atlaists no darba
    yt <- y2[v, ]
    yt$dienas <- as.numeric(difftime(t$beidz[2], t$sak[1], units = "days")) #bez '+ 1' jo ambiguous
  } else if (t$zinkod[2] == "25" && t$zinkod[3] == "11" && all(diff(t$NDZ_sanemsanas_datums) != 0)) {
    #Indivīds atsācis darbu pēc bezalgas atvaļinājuma, un tad tiek atlaists, tad atkal pieņemts darbā no jauna.
    # Nebrīnies par šo, ko kodi 80 un 81 ir izlaisti. 
    # Indivīds var tikt pieņemts darbā citādā formātā, 
    # piemēram, kā fiziska persona vai nosūtīts uz darbu ārzemēs.
    days1 <- as.numeric(difftime(t$beidz[2], t$sak[1], units = "days")) + 1 #jo darbs 
    days2 <- as.numeric(difftime(t$last_date[3], t$sak[3], units = "days"))  + 1 #jo darbs
    
    yt <- y2[v, ]
    yt$dienas <- sum(days1, days2)
    rm(days1, days2)
  } else if (t$zinkod[2] == "91" && t$zinkod[3] == "92" && all(diff(t$NDZ_sanemsanas_datums) != 0)) {
    #Indivīds atsācis darbu pēc bezalgas atvaļinājuma, aiziet bērnu kopšanas atvaļinājumā un tad atgriežas no tā.
    days1 <- as.numeric(difftime(t$beidz[2], t$sak[1], units = "days"))
    days2 <- as.numeric(difftime(t$last_date[3], t$sak[3], units = "days"))  + 1 #jo darbs
    
    yt <- y2[v, ]
    yt$dienas <- sum(days1, days2)
    rm(days1, days2)
  } else if (t$zinkod[2] == "25" && t$zinkod[3] == "51" && all(diff(t$NDZ_sanemsanas_datums[1:2]) != 0) && all(diff(t$NDZ_sanemsanas_datums[2:3]) == 0)) {
    #Indivīds atsācis darbu pēc bezalgas atvaļinājuma, bet tom'we nav atsācis, jo dienā, kad atsācis, tiek atlaists.
    # LĒMUMS: Darbinieks nostrādājis to vienu dienu, kad tika atlaists.
    yt <- y2[v, ]
    yt$dienas <- 1
  } else if (t$zinkod[2] == "50" && t$zinkod[3] == "21" && all(diff(t$NDZ_sanemsanas_datums[1:2]) == 0) && all(diff(t$NDZ_sanemsanas_datums[2:3]) != 0)) {
    #Dienā, kad indivīdam beidzas bezalgas atvaļinājums, tas paņem nākamo.
    #Tad aiziet no darba pēc paša vēlēšanās.
    yt <- y2[v, ]
    yt$dienas <- 0
  } else if (t$zinkod[2] == "53" && t$zinkod[3] == "54" && all(diff(t$NDZ_sanemsanas_datums) != 0)) {
    #Indivīds atsāk darbu pēc bezalgas atvaļinājuma, tad aiziet dīkstāvē un atgriāzas no tās.
    days1 <- as.numeric(difftime(t$beidz[2], t$sak[1], units = "days"))
    days2 <- as.numeric(difftime(t$last_date[3], t$sak[3], units = "days")) + 1
    
    yt <- y2[v, ]
    yt$dienas <- sum(days1,days2)
    rm(days1,days2)
  } else if (t$zinkod[2] == "51" && t$zinkod[3] == "25" && all(diff(t$NDZ_sanemsanas_datums) != 0)) {
    #Indivīds ar diviem kodiem atsāk darbu pēc bezalgas atvaļinājuma - tiek lietots vēlākais datums.
    #Tad tiek atlaists.
    yt <- y2[v, ]
    yt$dienas <- as.numeric(difftime(t$beidz[3], t$sak[2], units = "days")) + 1 #jo atlaišana
  } else {
    stop("Starpkodi3_51: Trūkst izstrādes koda.")
  }
  
  if(is.na(yt$PS_code[1])) {stop("Dienas NA.")}
  return(yt) 
}


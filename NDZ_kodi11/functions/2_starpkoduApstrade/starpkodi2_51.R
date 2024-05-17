starpkodi2_51 <- function(y2, t, prev, v) {
  if (t$zinkod[2] == "25") {
    yt <- y2[v,]
    yt$dienas <- as.numeric(difftime(t$beidz[2], t$sak[1], units = "days")) + 1
  } else if (t$zinkod[2] == "21") {
    # Indivīds sāk darbu pēc bezalgas atvaļinātjumā un tūlīt tiek atlaists.
    yt <- y2[v,]
    yt$dienas <- as.numeric(difftime(as.Date(t$beidz[2]), as.Date(t$sak)[1], units = "days"))
  } else if (t$zinkod[2] == "23") {
    # Darbinieks sāk darbu pēc bezalgas atvaļinājuma un tad uzņēmums tiek likvidēts.
    yt <- y2[v,]
    yt$dienas <- as.numeric(difftime(as.Date(t$beidz)[2], as.Date(t$sak)[1], units = "days"))
  } else if (t$zinkod[2] == "22") {
    # Darbinieks atsāk darbu pēc bezalgas atvaļinājuma, mazliet pastrādā vai nē un tiek atlaists.
    yt <- y2[v,] 
    yt$dienas <- as.numeric(difftime(as.Date(t$beidz[2]), as.Date(t$sak[1]), units = "days")) + 1
  } else if (t$zinkod[2] == "91") {
    # Darbinieks atsāk darbu pēc bezalgas atvaļinājuma un aiziet bērnu kopšanas atvaļinājumā.
    yt <- y2[v,] 
    yt$dienas <- as.numeric(difftime(as.Date(t$beidz[2]), as.Date(t$sak[1]), units = "days"))
  } else if (t$zinkod[2] == "41" && t$PS_code[1] == '______' && t$NM_code[1] == '___________') {
    #Šis ir unikāls gadījums, un domāju, ka te ir kļūda un "51" vietā jābūt "40" un atgiezās ar "41", citādi pēc plašākas uzpētes tur nesaprast
    days1 <- as.numeric(difftime(t$sak[1], prev, units = "days"))
    days2 <- as.numeric(difftime(t$last_date[2], t$sak[2], units = "days")) #tie divi t$sak abos aprēķinos NAV kļūda
    
    yt <- y2[v,] 
    yt$dienas <- sum(days1, days2)
    rm(days1, days2)
  } else if (t$zinkod[2] == "11" && t$PS_code[1] == '____________' && t$NM_code[1] == '__________') {
    #Šis ir unikāls gadījums
    #Dzīlāka izpēte rāda, ka darbinieks ar vairākiem 11 kodiem tiek pieņemts darbā, un izskatās, ka tas 51 ir kļūda no tiem,
    #bet arī nākamais 11 nav beidzamais, tāpēc uzskatu, ka darbinieks vēl nav pieņemts darbā.
    yt <- y2[v,] 
    yt$dienas <- 0
  } else if (t$zinkod[2] == "53") {
    #Indivīds atsāk darbu pēc bezalgas atvaļinājuma un tad aiziet dīkstāvē.
    yt <- y2[v,] 
    yt$dienas <- as.numeric(difftime(t$beidz[2], t$sak[1], units = "days"))
  } else {
    stop("starpkodi2_51: Trūkst izstrādes koda starpkodi2.")
  }
  
  if(is.na(yt$PS_code[1])) {stop("Dienas NA.")}
  return(yt)
}

#1 Funkcija occurencesSplit iziet cauri visai mēneša datnei un sadala to 
#  apakštabulās atkarībā no tā, cik reizes indivīds uzrādas dotajā mēnesī.
#  Šo tabulu nosaukumi iekabinās vektorā tabs_ndz, kur pēdējais cipars nosaukumā norāda
#  rindu skaitu, ko aizņem viens indivīds.
  result <- occurencesSplit(NDZ)
  tabs_ndz <- result$tabs_ndz
  NDZ_list <- result$NDZ_list
  rm(NDZ)

#2 Funkcija tempNDZ() izveido tukšu datni. 
#  Tā sevī uzkrās no apakštabulām, kas nāk result$NDZ_list, sarēķinātās tabulas ar dienu skaitu.
#  Vektors temp_rows dokumentē, cik rindas katrā solī tiek pievienotas datnei temp_NDZ.
#  (Tā ir pirmā riņķa tabula, kura nes gan pareizus dienu sarēķinus, gan tos, kas dublējas.)
  tempNDZ()
  
#3 No funkcijas occurencesSplit() atgriezto tabulu (NDZ_list) nosaukumos pēdējais cipars norāda, 
#  cik reizes vienā uzņēmumā strādājošais indivīds uzrādas dotajā kodu tabulā.
#  To nolasa un saglabā uz burta 'o' (no vārda 'occurences').
  
#3.1 Attiecīgi, ejot cauri tabulu nosaukumu vektoram tabs_ndz, Šis skripts nolasa o un ielādē atbilstošo tabulu.
#    To lieto tālākajā apstrādē.
  
for(i in 1:length(tabs_ndz)) {
# Ielādē attiecīgo tabulu  
  x <- NDZ_list[[i]]    
  
    if(nchar(tabs_ndz[i]) == 5) {
      o <- substr(tabs_ndz[i], 5, 5)
    } else if (nchar(tabs_ndz[i]) == 6) {
      o <- substr(tabs_ndz[i], 5, 6)
    } else {
      stop("Neatbilstošs tabulas nosaukuma garums")
    }
    
    switch(   
      o,
      # Caur sekojošo apstrādi sarēķina dienas un noglabā tabulā temp_NDZ, kuru būvē.
      "1"= sendTo_tempNDZ(processingOnes(x, o)),
      "2"= processingTwoes(x, o),  
      "3"= processingThrees(x, o),  
      "4"= processingFours(x, o), 
      "5"= processingFives(x, o),
      "6"= processingSixes(x, o), 
      "7"= processingSeven(x, o), 
      "8"= processingEights(x, o), # !! BLOĶĒTS processingEights_s4 testēšanai. Jāatbīvo lai ietu: tā nav kļūda.
      "9"= processingNines(x, o), 
      "10"= processingTens(x, o),
      "11"= processingEleven(x, o), #PALIKU TE
      "12"= processingTwelve(loadTable(o), o),
      "13"= processingThirteen(loadTable(o), o),
      "14"= processingFourteen(loadTable(o), o),
      "15"= processingFifteen(loadTable(o), o),
      "16"= processingSixteen(loadTable(o), o),
      "17"= processingSeventeen(loadTable(o), o),
      "18"= processingEighteen(loadTable(o), o),
      "19"= stop("NAV KODA PRIEKŠ NDZ_", o),
      "20"= stop("NAV KODA PRIEKŠ NDZ_", o),
      "21"= stop("NAV KODA PRIEKŠ NDZ_", o),
      "22"= stop("NAV KODA PRIEKŠ NDZ_", o),
      "23"= stop("NAV KODA PRIEKŠ NDZ_", o),
      "24"= processingTwentyFour(loadTable(o), o),
      "default" = stop("NAV KODA PRIEKŠ NDZ_", o))
  
  }    
  
  rm(NDZ_list, tabs_ndz, o, i)

  #----------------------------BŪVĒJAM PILNO MĒNESI
  
#1 Mēneša izstrādāto kodua tabulu sasummē uz duplikātiem.
  load("temp_NDZ.RData")
  x <- arrange(temp_NDZ, pseidokods, dnperk, nmrkod)
  
  cat("Kodu gala tabulā temp_NDZ pārbauda dubultos indivīdus.\n 
       Ja tādi ir, to sarēķinātās dienas summējas uz personu.\n")
  if(sum(duplicated(x[c("pseidokods", "dnperk", "nmrkod")])) > 0) {
    x <- x %>%
      group_by(period, pseidokods, dnperk, nmrkod) %>%
      summarise(
        dienas = sum(dienas, na.rm = TRUE)
      ) %>%
      arrange(pseidokods, dnperk, nmrkod)
    cat("Dubultnieki sasummēti.\n")
  } else {
    cat("Kodu gala tabulā dubultnieku nav.\n")
  }
  
#2 Pārbaude, vai kādam dienu skaits nav vairāk nekā mēnesī dienu.
if (nrow(x[x$dienas > 31, ]) == 0) { 
    cat("PĀRBAUDE IZIETA: Kodu gala tabulā nevienam nav 
      vairāk dienu par dienu skaitu mēnesī.\n")
} else {stop("STOP: Kad izstrādā šo, tad noņem STOP. 
           KODU GALA TABULĀ UZ UNIKĀLO INDIVĪDU SASUMMĒTĀS DIENAS 
           PĀRSNIEDZ DIENU SKAITU MĒNESĪ.\n")}
  
if(nrow(x[x$dienas < 0, ]) == 0) { 
  cat("PĀRBAUDE IZIETA: Kodu gala tabulā nevienam nav mazāk dienu par 0.\n")
} else {stop("STOP: Kad izstrādā šo, tad noņem STOP. 
           KODU GALA TABULĀ UZ UNIKĀLO INDIVĪDU SASUMMĒTĀS DIENAS 
           IR MAZĀK PAR NULLI.\n")}
  
#3 Ieliek nosaukumu un noglabā mēneša mapē.
assign(kodu_tab_nos, x) ##! assign nav laba priekš pakotnes
save(list = kodu_tab_nos, file = paste0(path, "data/intermediate_tables/buildingMonths/final_", kodu_tab_nos, ".RData"))
rm(list = kodu_tab_nos, kodu_tab_nos, temp_NDZ, x)
  
#4 Nokop aiz sevis.
#setwd(paste0(path, "data\\intermediate_tables\\"))
#file.remove("tabs_ndz.rds", "temp_rows.RDS", "temp_NDZ.RData")  

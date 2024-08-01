#1 Funkcija occurencesSplit iziet cauri visai mēneša datnei un sadala to 
#  apakštabulās atkarībā no tā, cik reizes indivīds uzrādas dotajā mēnesī.
#  Šo tabulu nosaukumi iekabinās vektorā tabs_ndz, kur pēdējais cipars nosaukumā norāda
#  rindu skaitu, ko aizņem viens indivīds.
  result <- occurencesSplit(NDZ)
  tabs_ndz <- result$tabs_ndz
  NDZ_list <- result$NDZ_list
  rm(NDZ)

#2 Funkcija tempNDZ izveido tukšu datu rāmi. 
#  Tā sevī uzkrās no apakštabulām sarēķinātās tabulas ar dienu skaitu.
#  Vektors temp_rows dokumentē, cik rindas katrā solī tiek pievienotas datnei temp_NDZ.
#  (Tā ir pirmā riņķa tabula, kurā būs gan pareizi sarēķini, gan tie, kas dublēsies.)  
  tNDZ <- tempNDZ()
  temp_NDZ <- tNDZ$temp_NDZ
  temp_rows <- tNDZ$temp_rows
  rm(tNDZ)
  

#3 No funkcijas occurencesSplit atgriezto tabulu nosaukumos pēdējais cipars norāda, 
#  cik reizes vienā uzņēmumā strādājošais indivīds uzrādas dotajā tabulā.
#  To nolasa un saglabā uz burta 'o' (no vārda 'occurences').
  
#3.1 Attiecīgi, ejot cauri tabulu nosaukumu vektoram tabs_ndz, nolasa o un ielādē atbilstošo tabulu.
#    Vadoties pēc, to sūta tālāk uz apstrādi.

 #i <- 1 #testēšanai
#for(i in 1:length(tabs_ndz)) {
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
      "1"= temp_NDZ <- sendTo_tempNDZ(processingOnes(x, o), temp_NDZ),
      "2"= processingTwoes(loadTable(o), o),
      "3"= processingThrees(loadTable(o), o),
      "4"= processingFours(loadTable(o), o),
      "5"= processingFives(loadTable(o), o),
      "6"= processingSixes(loadTable(o), o),
      "7"= processingSeven(loadTable(o), o),
      "8"= processingEights(loadTable(o), o),
      "9"= processingNines(loadTable(o), o),
      "10"= processingTens(loadTable(o), o),
      "11"= processingEleven(loadTable(o), o),
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
  
  temp_rows <- append(temp_rows, nrow(temp_NDZ) - sum(temp_rows))
  }    
  
  rm(NDZ_list, tabs_ndz, o, i)

  #----------------------------BŪVĒJAM PILNO MĒNESI
  
  #1. Paņemam mēneša izstrādātos kodus un savelkam tos vēlreiz uz duplikātiem.
  
  load("temp_NDZ.RData")
  x <- temp_NDZ
  x <- x[order(x$PS_code, x$DN_code, x$NM_code), ]
  
  cat("Kodu gala tabulā temp_NDZ pārbauda dubultos indivīdus, ja ir, tad sasummē to dienas.\n")
  if(sum(duplicated(x[c("PS_code", "DN_code", "NM_code")])) > 0) {
    x <- x %>%
      group_by(period, PS_code, DN_code, NM_code) %>%
      summarise(
        dienas = sum(dienas, na.rm = TRUE)
      ) %>%
      arrange(PS_code)
    cat("Dubultnieki sasummēti.\n")
  } else {
    cat("Kodu gala tabulā dubultnieku nav.\n")
  }
  
  #2. Pārbauda, vai kādam dienu skaits ir vairāk nekā mēnesī dienu.
  if (nrow(x[x$dienas > 31, ]) == 0) { #TODO: Te priekš tā 31, vajag iztrādāt funkciju, kur, vadoties pēc perioda ailes, tiek pānemts datums salīdzināšanai.
    cat("PĀRBAUDE IZIETA: Kodu gala tabulā nevienam nav 
      vairāk dienu par dienu skaitu mēnesī.\n")
  } else {
    stop(cat("STOP: Kad izstrādā šo, tad noņem STOP. 
           KODU GALA TABULĀ UZ UNIKĀLO INDIVĪDU SASUMMĒTĀS DIENAS 
           PĀRSNIEDZ DIENU SKAITU MĒNESĪ.\n"))
  }
  
  if (nrow(x[x$dienas < 0, ]) == 0) { #TODO: Te priekš tā 31, vajag iztrādāt funkciju, kur, vadoties pēc perioda ailes, tiek pānemts datums salīdzināšanai.
    cat("PĀRBAUDE IZIETA: Kodu gala tabulā nevienam nav 
      mazāk dienu par 0.\n")
  } else {
    stop(cat("STOP: Kad izstrādā šo, tad noņem STOP. 
           KODU GALA TABULĀ UZ UNIKĀLO INDIVĪDU SASUMMĒTĀS DIENAS 
           IR MAZĀK PAR NULLI.\n"))
  }
  
  #3. Ieliec nosaukumu un noglabā mēneša mapē.
  assign(kodu_tab_nos, x)
  setwd(paste0(path, "data\\intermediate_tables\\buildingMonths"))
  save(list = kodu_tab_nos, file = paste0("final_", kodu_tab_nos, ".RData"))
  
  rm(list = kodu_tab_nos, kodu_tab_nos, temp_NDZ, x)
  
  
  #4. Nokop aiz sevis.
  setwd(paste0(path, "data\\intermediate_tables\\"))
  file.remove("tabs_ndz.rds", "temp_rows.RDS", "temp_NDZ.RData")  

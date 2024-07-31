####kodu_tab_nos <- "NDZ202310_11" #74184
####kodu_tab_nos <- "NDZ202201_40"
####kodu_tab_nos <- "NDZ202201_50" #16020
#kodu_tab_nos <- "NDZ202201_53" #448

#for(kods in kodu_vektors) {
#  kodu_tab_nos <- paste0("NDZ202201_", kods)

# Sekojošo pārcel uz csv ielādi vai datubāzi, kad tās sadaļas gatavas
  setwd(paste0(path, "data\\originals\\", year))
  load(paste0("starting_", kodu_tab_nos, ".RData"))
  NDZ <- get(kodu_tab_nos)
  rm(list = kodu_tab_nos)  

  tabs_ndz <- occurencesSplit(NDZ)
  rm(NDZ)
  
  setwd(paste0(path, "data\\intermediate_tables\\"))
  tempNDZ()
  
  #tabs_ndz <- readRDS("tabs_ndz.rds")
  #Tabulu pēdējais cipars norāda uz to, cik reizes vienā uzņēmumā strādājošs indivīds uzrādas
  #dotajā tabulā. Mēs nolasām to un saglabājam uz burta 'o' (no vārda 'occurences')

for(i in 1:length(tabs_ndz)) {
    if(nchar(tabs_ndz[i]) == 5) {
      o <- substr(tabs_ndz[i], 5, 5)
    } else if (nchar(tabs_ndz[i]) == 6) {
      o <- substr(tabs_ndz[i], 5, 6)
    } else {
      stop("Neatbilstošs tabulas nosaukuma garums") #TODO Izformē šo
    }

    o <- as.character(o)
    
    switch(   
      o,
      #1. Sarēķini dienas un noglabā gala tabulā, kuru būvē.
      "1"= sendTo_tempNDZ(processingOnes(loadTable(o), o)),
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
  
    #Nokop aiz sevis.
    file_name <- paste0("NDZ_", o, ".RData")
    if (file.exists(file_name)) {
      file.remove(file_name)
      print(paste("File", file_name, "deleted successfully./n"))
    } else {
      print(paste("File", file_name, "does not exist."))
    }
    rm(file_name)
  }    
  
  rm(tabs_ndz, o, i)

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
    cat("Dubltnieki sasummēti.\n")
  } else {
    cat("Kodu gala tabulā dubltnieku nav.\n")
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
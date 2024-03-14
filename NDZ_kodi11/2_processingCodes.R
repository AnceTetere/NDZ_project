kodu_vektors <- c("11", "40", "50", "53", "55", "81", "91")

for(kods in kodu_vektors) {
  kodu_tab_nos <- paste0("NDZ202201_", kods)
  
# Sekojošo pārcel uz csv ielādi vai datubāzi, kad tās sadaļas gatavas
  setwd(paste0(path, "data\\originals\\"))
  load(paste0("starting_", kodu_tab_nos, ".RData"))
  NDZ <- get(kodu_tab_nos)
  rm(list = kodu_tab_nos)  

  tabs_NDZ <- occurencesSplit(NDZ)
  
  setwd(paste0(path, "data\\intermediate_tables\\"))
  tempNDZ()
  
  for(i in 1:length(tabs_NDZ)) {
    if(nchar(tabs_NDZ[i]) == 5) {
      o <- substr(tabs_NDZ[i], 5, 5)
    } else if (nchar(tabs_NDZ[i]) == 6) {
      o <- substr(tabs_NDZ[i], 5, 6)
    } else {
      print("Neatbilstošs tabulas nosaukuma garums") #TODO Izformē šo
    }
    
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
      "12"= processingTwelwe(loadTable(o), o),
      "13"= processingThirteen(loadTable(o), o),
      "16"= processingSixteen(loadTable(o), o),
      "17"= processingSeventeen(loadTable(o), o),
      "18"= processingEighteen(loadTable(o), o))
    
    #Nokop aiz sevis.
    file_name <- paste0("NDZ_", o, ".RData")
    if (file.exists(file_name)) {
      file.remove(file_name)
      print(paste("File", file_name, "deleted successfully."))
    } else {
      print(paste("File", file_name, "does not exist."))
    }
    rm(file_name)
  }    
  
  rm(tabs_NDZ, o, i)

  #----------------------------BŪVĒJAM PILNO MĒNESI
  
  #1. Paņemam mēneša izstrādātos kodus un savelkam tos vēlreiz uz duplikātiem.
  
  load("temp_NDZ.RData")
  x <- temp_NDZ
  x <- x[order(x$PS_code, x$DN_code, x$NM_code), ]
  

  if(sum(duplicated(temp_NDZ[c("PS_code", "DN_code", "NM_code")])) > 0) {
    x <- x %>%
      group_by(period, PS_code, DN_code, NM_code) %>%
      summarise(
        dienas = sum(dienas, na.rm = TRUE)
      ) %>%
      arrange(PS_code)
  } else {
    cat("Kodu gala tabulā dubltnieku nav.\n")
  }
  
  #2. Pārbauda, vai kādam dienu skaits ir vairāk nekā mēnesī dienu.
  if (nrow(x[x$dienas > 31, ]) == 0) { #TODO: Te priekš tā 31, vajag iztrādāt funkciju, kur, vadoties pēc perioda ailes, tiek pānemts datums salīdzināšanai.
    cat("PĀRBAUDE IZIETA: Kodu gala tabulā nav 
      vairāk dienu par dienu skaitu mēnesī.\n")
  } else {
    stop(cat("STOP: SUMMĒTĀS DIENAS 
           PĀRSNIEDZ DIENU SKAITU MĒNESĪ.\n"))
  }
  
  #3. Ieliec nosaukumu un noglabā mēneša mapē.
  assign(kodu_tab_nos, x)
  setwd(paste0(path, "data\\intermediate_tables\\buildingMonths"))
  save(list = kodu_tab_nos, file = paste0("final_", kodu_tab_nos, ".RData"))
  
  rm(list = kodu_tab_nos, kodu_tab_nos, temp_NDZ, x)
  
  
  #4. Nokop aiz sevis.
  setwd(paste0(path, "data\\intermediate_tables\\"))
  file.remove("tabs_NDZ.rds", "temp_rows.RDS", "temp_NDZ.RData")  
  }

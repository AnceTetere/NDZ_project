processingCodes <- function(NDZ, kods) {
  result <- occurencesSplit(NDZ)
  tabs_zdn <- result$tabs_zdn
  NDZ_list <- result$NDZ_list
  rm(NDZ, result)

#2 Funkcija tempNDZ() izveido tukšu datni. 
  tempNDZ()
  
#i <- 5 #testēšanai
#for(i in 1:4) { #TESTIEM
    
for(i in 1:length(tabs_zdn)) {
  x <- NDZ_list[[i]]    ### loadTables nekļūst lieka, jo tiek lietota 1-majā skriptā - skati vai to var aizvietot <- Ievēro, ja tu šādi lieto, funkcija load_table() kļūst lieka

      if(nchar(tabs_zdn[i]) == 5) {
      o <- substr(tabs_zdn[i], 5, 5)
    } else if (nchar(tabs_zdn[i]) == 6) {
      o <- substr(tabs_zdn[i], 5, 6)
    } else {
      stop("Neatbilstošs tabulas nosaukuma garums")
    }
    
  cat("------------------- o =", o, "\n")
  
    switch(   
      o,
      "1"= cat(sendTo_tempNDZ(processingOnes(x, o), o)),
      "2"= processingTwoes(x, o, kods),  
      "3"= processingThrees(x, o, kods),  
      "4"= processingFours(x, o, kods), 
      "5"= processingFives(x, o, kods),
      "6"= processingSixes(x, o, kods), 
      "7"= processingSeven(x, o, kods), 
      "8"= processingEights(x, o, kods), # !! BLOĶĒTS processingEights_s4 testēšanai. Jāatbrīvo, lai ietu: tā nav kļūda.
      "9"= processingNines(x, o, kods),
      "10"= processingTens(x, o, kods),
      "11"= processingEleven(x, o, kods), 
      "12"= processingTwelve(x, o, kods),
      "13"= processingThirteen(x, o, kods), #PALIKU TE
      "14"= processingFourteen(x, o, kods),
      "15"= processingFifteen(x, o, kods),
      "16"= processingSixteen(x, o, kods),
      "17"= processingSeventeen(x, o, kods),
      "18"= processingEighteen(x, o, kods),
      "19"= stop("NAV KODA PRIEKŠ NDZ_", o),
      "20"= stop("NAV KODA PRIEKŠ NDZ_", o),
      "21"= stop("NAV KODA PRIEKŠ NDZ_", o),
      "22"= stop("NAV KODA PRIEKŠ NDZ_", o),
      "23"= stop("NAV KODA PRIEKŠ NDZ_", o),
      "24"= processingTwentyFour(loadTable(o), o),
      "default" = stop("NAV KODA PRIEKŠ NDZ_", o))
  }    
  
  rm(NDZ_list, tabs_zdn, o, i, x)
  return(cat("Kodu kopums", kods, "apstrādāts."))
}

#Kods tiek ņemts no tabs_zdn vektora.
setwd(paste0(path, "data\\intermediate_tables\\"))
tempZDN()
#tabs_zdn <- readRDS("tabs_zdn.rds")

for(i in 1:length(tabs_zdn)) {
  if(nchar(tabs_zdn[i]) == 5) {
    o <- substr(tabs_zdn[i], 5, 5)
  } else if (nchar(tabs_zdn[i]) == 6) {
    o <- substr(tabs_zdn[i], 5, 6)
  } else {
    print("Neatbilstošs tabulas nosaukuma garums") #TODO Izformē šo
  }

switch(   
    o,
    "1"= sendTo_tempZDN(processingOnes(loadTable(o), o)),
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
    "12"= cat("12-nieki NAV IZSTRĀDĀTI"),
    "13"= processingThirteen(loadTable(o), o),
    "18"= processingEighteen(loadTable(o), o))

#Nokop aiz sevis.
    file_name <- paste0("ZDN_", o, ".RData")
  if (file.exists(file_name)) {
    file.remove(file_name)
    print(paste("File", file_name, "deleted successfully."))
  } else {
    print(paste("File", file_name, "does not exist."))
  }
  rm(file_name)
}    
#readRDS("temp_rows.RDS")
#load("temp_ZDN.RData")
rm(tabs_zdn, o, i)

#----------------------------BŪVĒJAM PILNO DATNI

#1. Izstrādātos kodus vēlreiz savelk uz duplikātiem.

load("temp_ZDN.RData")
x <- temp_ZDN
x <- x[order(x$ps_code, x$dn_code, x$nm_code), ]

cat("Kodu gala tabulā temp_ZDN pārbauda dubultās vienības; ja ir, sasummē dienas.\n")
if(sum(duplicated(temp_ZDN[c("ps_code", "nm_code")])) > 0) {
  x <- x %>%
    group_by(period, ps_code, dn_code, nm_code) %>%
    summarise(
      dienas = sum(dienas, na.rm = TRUE)
    ) %>%
    arrange(ps_code)
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

#3. Ieliec nosaukumu un noglab
assign(kodu_tab_nos, x)
setwd(paste0(path, "data\\intermediate_tables\\building"))
save(list = kodu_tab_nos, file = paste0("final_", kodu_tab_nos, ".RData"))

rm(list = kodu_tab_nos, kodu_tab_nos, temp_ZDN, x)

#4. Nokop aiz sevis.
setwd(paste0(path, "data\\intermediate_tables\\"))
file.remove("tabs_zdn.rds", "temp_rows.RDS", "temp_ZDN.RData")


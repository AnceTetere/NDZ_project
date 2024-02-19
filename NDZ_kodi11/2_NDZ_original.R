
library(dplyr)
path <- "..\\processing_NDZ_kodi11\\"
source(paste0(path, "R\\functions\\loadTable.R"))
source(paste0(path, "R\\functions\\processingOnes.R"))
source(paste0(path, "R\\functions\\codes_match.R"))
source(paste0(path, "R\\functions\\cancelOnes.R"))
source(paste0(path, "R\\functions\\1_doubleStartEnd_codesMatch_function.R"))
source(paste0(path, "R\\functions\\2_doubleStartEnd_codesDiffer_algorythm.R"))
source(paste0(path, "R\\functions\\processingTwoes.R"))
source(paste0(path, "R\\functions\\doublesTest.R"))
source(paste0(path, "R\\functions\\processingDoubles.R"))
source(paste0(path, "R\\functions\\processingThrees.R"))
source(paste0(path, "R\\functions\\splittingThrees.R"))
source(paste0(path, "R\\functions\\processingFours.R"))
source(paste0(path, "R\\functions\\processingFives.R"))
source(paste0(path, "R\\functions\\processingSixes.R"))
source(paste0(path, "R\\functions\\processingSeven.R"))
source(paste0(path, "R\\functions\\processingEights.R"))
source(paste0(path, "R\\functions\\processingNines.R"))
source(paste0(path, "R\\functions\\processingTens.R"))
source(paste0(path, "R\\functions\\processingEleven.R"))
source(paste0(path, "R\\functions\\processingThirteen.R"))
source(paste0(path, "R\\functions\\processingEighteen.R"))
source(paste0(path, "R\\functions\\create_tempNDZ.R"))
source(paste0(path, "R\\functions\\sendTo_tempNDZ.R"))

# Mēneša tabula tiek apstrādāta pa kodiem, kas pēc eksportēti lietojot SQL_export

#1 Noskaidro, par kuriem kodiem tiks veikts dienu aprēķins mēnesī.
#KODI: NDZ202201_xx
#KODI: NDZ202201_xx
#KODI: NDZ202201_xx

  
#kodu_tab_nos <- "NDZ202201_xx" 
#kodu_tab_nos <- "NDZ202201_xx" 
#kodu_tab_nos <- "NDZ202201_xx"
#load(paste0("starting_", kodu_tab_nos, ".RData"))
#NDZ <- get(kodu_tab_nos)
#rm(list = kodu_tab_nos)


#2 Ielādē attiecīgo tabulu
setwd(paste0(path, "data\\originals\\"))
NDZ <- read.table(paste0(kodu_tab_nos, ".csv"), header = TRUE, sep = ";") 

#3. Izformē ailes.
NDZ$period <- as.character(NDZ$period)
NDZ$dn_code <- as.character(NDZ$dn_code)
NDZ$zinkod_sak <- as.character(NDZ$zinkod_sak)
NDZ$sak_darbu <- as.Date(NDZ$sak_darbu)
NDZ$zinkod_beidz <- as.character(NDZ$zinkod_beidz)
NDZ$beidz_darbu <- as.Date(NDZ$beidz_darbu)
NDZ$zinkod <- as.character(NDZ$zinkod)
NDZ$NDZ_sanemsanas_datums <- as.Date(NDZ$NDZ_sanemsanas_datums)

#pārkodējam ziņu kodus ailēs [start] un [end] 
NDZ$start[NDZ$zinkod_sak %in% c(11, 13, 14, 61, 81, 41)] <- "1"
NDZ$end[NDZ$zinkod_beidz %in% c(21, 22, 23, 24, 25, 29, 82, 40)] <- "2"
NDZ$end[NDZ$zinkod_beidz == "26"] <- "26"

if(sum(NDZ$zinkod_beidz == "26", na.rm = TRUE) > 0) {
  stop(cat("Mēneša kodu tabulā ir kods vēl nav izstrādāts."))
}
  
NDZ$zinkod_sak <- NULL
NDZ$zinkod_beidz <- NULL

NDZ$start[is.na(NDZ$start)] <- ""
NDZ$dn_code[is.na(NDZ$dn_code)] <- ""
NDZ$end[is.na(NDZ$end)] <- ""

#sakārto tabulā ailes
NDZ <- NDZ[ , c("period", "ps_code", "dn_code", "nm_code", "start", "sak_darbu", "end", "beidz_darbu", "prof", "zinkod", "NDZ_sanemsanas_datums", "last_date", "dienas")]
assign(kodu_tab_nos, NDZ)

#Saglabā eksportēto failu lietošanai ar R
save(list = kodu_tab_nos, file = paste0("starting_", kodu_tab_nos, ".RData"))
rm(list = kodu_tab_nos)

#Izdzēs no MS SQL eksportēto failu

file_name <- paste0(kodu_tab_nos, ".csv")
if (file.exists(file_name)) {
  file.remove(file_name)
  print(paste("File", file_name, "deleted successfully."))
} else {
  print(paste("File", file_name, "does not exist."))
}
rm(file_name)

# Mēneša tabula tiek apstrādāta pa kodiem, 
# kas pēc SQL_export koda eksportēti no
# MS SQL datubāzes NDZYYYYMM

#1 Noskaidro, par kuriem kodiem tiks veikts dienu aprēķins mēnesī.
#KODI: NDZ202201_11
#KODI: NDZ202201_40
#KODI: NDZ202201_50
#KODI: NDZ202201_53
#KODI: NDZ202201_55 IZŅEMU skat kodu aprakstus
#KODI: NDZ202201_81 IZŅEMU, jo norāda nodarbinātības formu
#KODI: NDZ202201_91 Izņemts

# Rindas 15 - 18: PAGAIDU TESTĒŠANAI
#kodu_tab_nos <- "NDZ202201_11"
#kodu_tab_nos <- "NDZ202201_40"
#kodu_tab_nos <- "NDZ202201_50"
#kodu_tab_nos <- "NDZ202201_53"

kodu_tab_nos <- paste0("NDZ", year, month, "_", kods)

#2 Ielādē attiecīgo tabulu
setwd(paste0(path, "data\\originals\\", year))
NDZ <- read.table(paste0(kodu_tab_nos, ".csv"), header = TRUE, sep = ";", 
                  colClasses = c("character", "character", "character", 
                                 "character", "character", "Date", 
                                 "character", "Date", "character", 
                                 "character", "Date", "Date", "character"))

#TODO: If you don't have the data in a data frame, you can read it from a SQL Server
# database using the 'odbc' or 'RODBC' packages and then proceed with the code above.

#3. Izformē ailes.
#pārkodējam ziņu kodus ailēs [start] un [end] 
NDZ$start[NDZ$zinkod_sak %in% c(11, 14, 61, 41, 51, 54, 92)] <- "1"
NDZ$end[NDZ$zinkod_beidz %in% c(21, 22, 23, 24, 25, 26, 29, 40, 50, 53, 91)] <- "2"

NDZ$zinkod_sak <- NULL
NDZ$zinkod_beidz <- NULL

NDZ$start[is.na(NDZ$start)] <- ""
NDZ$DN_code[is.na(NDZ$DN_code)] <- ""
NDZ$end[is.na(NDZ$end)] <- ""
NDZ$dienas <- as.integer(NDZ$dienas)

#sakārto tabulā ailes
NDZ <- NDZ[ , c("period", "PS_code", "DN_code", "NM_code", "start", "sak", "end", "beidz", "prof", "zinkod", "NDZ_sanemsanas_datums", "last_date", "dienas")]
assign(kodu_tab_nos, NDZ)

#Saglabā eksportēto failu lietošanai ar R
save(list = kodu_tab_nos, file = paste0("starting_", kodu_tab_nos, ".RData"))
rm(list = kodu_tab_nos)

#Izdzēs no MS SQL eksportēto failu
#Eksports notika ar ..\SQL\1_export_NDZ202201_11etc.sql
file_name <- paste0(kodu_tab_nos, ".csv")
if (file.exists(file_name)) {
  file.remove(file_name)
  print(paste("File", file_name, "deleted successfully."))
} else {
  print(paste("File", file_name, "does not exist."))
}
rm(file_name)

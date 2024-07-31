# Mēneša tabula pēc SQL_export

#1 Definē mēneša tabulas nosaukumu
kodu_tab_nos <- paste0("NDZ", year, month)

#2 Ielādē attiecīgo tabulu
setwd(paste0(path, "data\\originals\\", year))
NDZ <- read.table(paste0(kodu_tab_nos, ".csv"), header = TRUE, sep = ";", 
                  colClasses = c("character", "character", "character", 
                                 "character", "character", "Date", 
                                 "character", "Date"))

#TODO: If you don't have the data in a data frame, you can read it from a SQL Server database using the 'odbc' or 'RODBC' packages and then proceed with the code above.

#3 Maza pārbaude (varbūt nevajag)
if (sum(NDZ$period != paste0(year, month)) == 0) {
  NDZ$DN_code[is.na(NDZ$DN_code)] <- ""
  NDZ$PS_code[is.na(NDZ$PS_code)] <- ""
  A <- c("NM_code", "sak_beidz", "NDZ_sanemsanas_datums", "zinkod", "last_date") 
  for(a in A) {
    if (sum(is.na(NDZ[ , a])) != 0) {stop("Ailē", a, "iztrūkst vērtības (skat. 1_NDZ_original.R)")}
  }
  rm(A, a)
} else {
  stop("Vērtības ailē period nesakrīt ar definēto gadu un mēnesi.")
}

#4 Pievieno aili dienas
NDZ$dienas <- as.integer(0)

#Dod nosaukumu RData tabulai
assign(kodu_tab_nos, NDZ)

#Izdzēs no MS SQL eksportēto failu
#Eksport notika uz ..NDZ_codes\data\originals\yyyy (yyyy - gads)
file_name <- paste0(kodu_tab_nos, ".csv")
if (file.exists(file_name)) {
  file.remove(file_name)
  print(paste("File", file_name, "deleted successfully."))
} else {
  print(paste("File", file_name, "does not exist."))
}
rm(file_name, NDZ)

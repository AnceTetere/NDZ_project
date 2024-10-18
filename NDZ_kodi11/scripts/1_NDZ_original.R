#Te izmanto mēneša kodu tabula pēc SQL_export no data/originals

#1 Definē mēneša tabulas nosaukumu
kodu_tab_nos <- paste0("NDZ", year, month, "_", kods)

#2 Ielādē attiecīgo tabulu
NDZ <- read.table(paste0(path, "data/originals/", year, "/", kodu_tab_nos, ".csv"), 
                  header = TRUE, sep = ";", 
                  colClasses = c("character", "character", "character", 
                                 "character", "character", "Date", 
                                 "character", "Date"))

#TODO: If you don't have the data in a data frame, you can read it from a SQL Server database using the 'odbc' or 'RODBC' packages and then proceed with the code above.

if (nrow(NDZ) > 0) {
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
  
  #5 Izdzēs no MS SQL eksportēto failu
  #Eksport notika uz ..NDZ_codes\data\originals\yyyy (yyyy - gads)
  file_name <- paste0(paste0(path, "data\\originals\\", year, "\\", kodu_tab_nos, ".csv"))
  #TESTĒŠANAI - ATBRĪVO PĒC TAM
  #if (file.exists(file_name)) {
  #  file.remove(file_name)
  #  print(paste("File", file_name, "deleted successfully."))
  #} else {
  #  print(paste("File", file_name, "does not exist."))
  #}
  rm(file_name)
} else {
  paste0("Šajā mēnesī ", year, month, " nav neviena ieraksta kodu komplektam: ", kods, ".")
}

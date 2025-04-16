NDZ_original <- function(kods) {
  
  #1 Ielādē datus
  NDZ <- NDZ_readDB(kods)

  #2 Pārbaude vai attiecīgajā tabulā ir datu,
  #  tad pārbauda vai [period] atribūts atbilst. 
  if (nrow(NDZ) > 0) {
    if (sum(NDZ$period != paste0(year, month)) == 0) {
      #Tad izņem NAs.
        NDZ$dnperk[is.na(NDZ$dnperk)] <- ""
        NDZ$PS_code[is.na(NDZ$PS_code)] <- ""
        A <- c("NM_code", "sak_beidz", "NDZ_sanemsanas_datums", "zinkod", "last_date") 
        for(a in A) {
          if (sum(is.na(NDZ[ , a])) != 0) {stop("NDZ_original: Ailē", a, "iztrūkst vērtības.")}
        }
        rm(A, a)
  } else {stop("Vērtības ailē period nesakrīt ar definēto gadu un mēnesi.")}
  
  #4 Pievieno aili dienas
  NDZ$dienas <- as.integer(0)
  
  #5 Te ir piezīmes no iepriekšējām apstrādēm, kas iztīra datni tālāk.
  NDZ <- adj50(NDZ)
 
  #6 Saglabā eksportēto failu lietošanai ar starpkodu izstrādē.
  #1 Mēneša tabulas nosaukums
  kodu_tab_nos <- paste0("NDZ", year, month, "_", kods)
  assign(kodu_tab_nos, NDZ, envir = environment())
  
  if (!dir.exists("data/starptabulas/")) {dir.create("data/starptabulas/")}
  if (!dir.exists(paste0("data/starptabulas/", year, "/"))) {dir.create(paste0("data/starptabulas/", year, "/"))}
  save(list = kodu_tab_nos, file = paste0("data/starptabulas/", year, "/starting_", kodu_tab_nos, ".RData"))
  rm(list = kodu_tab_nos) 
  
  #5 Izdzēs no MS SQL eksportēto failu
  #Eksports notika uz ../data/originals/yyyy (kur yyyy - gads)
  #file_name <- paste0("data/originals/", year, "/", kodu_tab_nos, ".csv"))
  #if (file.exists(file_name)) {
  #  file.remove(file_name)
  #  print(paste("Orģinālu datne", file_name, "dzēsta."))
  #} else {
  #  print(paste("Orģinālu datne", file_name, "netika atrasta."))
  #}
  #rm(file_name)
} else {
  message("Mēnesī ", year, month, " nav neviena ieraksta kodu komplektam: ", kods, ".")
}

rm(kods, kodu_tab_nos)
return(NDZ)
}

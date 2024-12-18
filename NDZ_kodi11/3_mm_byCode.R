load(paste0("data/starptabulas/", year, "/temp_NDZ.RData"))
x <- arrange(temp_NDZ, PS_code, DN_code, NM_code)

if(sum(duplicated(x[c("PS_code", "DN_code", "NM_code")])) > 0) {
  x <- x %>%
    group_by(period, PS_code, DN_code, NM_code) %>%
    summarise(dienas = sum(dienas, na.rm = TRUE)) %>%
    arrange(PS_code, DN_code, NM_code)
  cat("Dubultnieki sasummēti.\n")
} else {
  cat("Kodu gala tabulā dubultnieku nav.\n")
}

#KONTROLES    
md <- mm_dienu_skaits(year, MM)

if (nrow(x[x$dienas > md, ]) == 0) {
  cat("PĀRBAUDE IZIETA.\n")
} else {stop()}

if(nrow(x[x$dienas < 0, ]) == 0) {
  cat("PĀRBAUDE IZIETA.\n")
} else {stop()}

kodu_tab_nos <- paste0("NDZ", year, MM, "_", kods)
assign(kodu_tab_nos, x, envir = environment())

if(!dir.exists("data/starptabulas/buildingMM")) {dir.create("data/starptabulas/buildingMM")}
save(list = kodu_tab_nos, file = paste0("data/starptabulas/buildingMM/final_", kodu_tab_nos, ".RData"))

rm(list = kodu_tab_nos, kodu_tab_nos, temp_NDZ, x)

#4 Nokop aiz sevis.
#CENTOS NEKO NESAGLABĀT, TAČU temp_rows un temp_NDZ NĀK NO FUNKCIJAS tempNDZ().
#Izdomā vēlāk kā šo pārstrādāt.
#list.files(paste0(path, "data/intermediate_tables/"), full.names = TRUE)
file.remove(paste0("data/starptabulas/", year, "/temp_rows.RDS"), paste0("data/starptabulas/", year, "/temp_NDZ.RData"))

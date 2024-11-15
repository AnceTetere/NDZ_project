#---------------------------- BŪVĒJAM PILNO MĒNESI --------------
#1 Mēneša izstrādāto kodus tabulu sasummē uz duplikātiem.
load("temp_ZDN.RData")
x <- arrange(temp_ZDN, PS_code, DN_code, NM_code)


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
md <- menesa_dienu_skaits(year, month)

if (nrow(x[x$dienas > md, ]) == 0) {
  cat("PĀRBAUDE IZIETA: Koda gala tabulā nevienam nav vairāk dienu par dienu skaitu mēnesī.\n")
} else {stop("KODU GALA TABULĀ UZ UNIKĀLO INDIVĪDU SASUMMĒTĀS DIENAS PĀRSNIEDZ MĒNEŠA DIENU SKAITU.\n")}

if(nrow(x[x$dienas < 0, ]) == 0) {
  cat("PĀRBAUDE IZIETA: Kodu gala tabulā nevienam nav mazāk dienu par 0.\n")
} else {stop("KODU GALA TABULAS AILĒ \"DIENAS\" IR NEGATĪVAS VĒRTĪBAS.\n")}

assign(kodu_tab_nos, x, envir = environment())#Izmanto env definējumu, lai pēcāk var iestrādāt pakotnē.
save(list = kodu_tab_nos, file = paste0(path, "data/intermediate_tables/buildingMonths/final_", kodu_tab_nos, ".RData"))
rm(list = kodu_tab_nos, kodu_tab_nos, temp_ZDN, x)

#4 Nokop aiz sevis.
#Izdomā vēlāk kā šo pārstrādāt.
#list.files(paste0(path, "data/intermediate_tables/"), full.names = TRUE)
file.remove("temp_rows.RDS", "temp_ZDN.RData")

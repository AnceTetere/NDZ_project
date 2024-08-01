#Funkcija sendTo_tempNDZ() sasummē dienas uz oriģinālo indivīdu mēnesī,
#veic papildus pārbaudes un pievieno apstrādātās rindas izstrādes tabulai temp_NDZ, ko šis kods būvē.

sendTo_tempNDZ <- function(x) {
  x <- x[order(x$PS_code, x$DN_code, x$NM_code, x$NDZ_sanemsanas_datums, x$sak_beidz), ]
  x <- x[ , c("period", "PS_code", "DN_code", "NM_code", "dienas")]
  
  x <- x %>%
    group_by(period, PS_code, DN_code, NM_code) %>%
    summarise(
      dienas = sum(dienas, na.rm = TRUE)
    ) %>%
    arrange(PS_code)

  if (nrow(x[duplicated(x[c("PS_code", "NM_code")]), ]) == 0 && sum(x$dienas < 0) == 0) {
    #setwd(paste0(path, "data\\intermediate_tables\\"))
    #load("temp_NDZ.RData")
    
    temp_NDZ <- rbind(temp_NDZ, x[, c("period", "PS_code", "DN_code", "NM_code", "dienas")])
    #save(temp_NDZ, file = "temp_NDZ.RData")
    
    #temp_rows <- readRDS("temp_rows.rds")
    temp_rows <- append(temp_rows, nrow(x))
    #saveRDS(temp_rows, file = "temp_rows.RDS")
    #rm(temp_NDZ, x, temp_rows)
    rm(x)
  } else {
    stop("Tabulā ir dubultnieki vai negatīvas dienas. o ir", o, ".\n")
  }
}

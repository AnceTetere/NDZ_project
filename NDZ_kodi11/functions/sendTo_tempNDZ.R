sendTo_tempNDZ <- function(x) {
  x <- x[order(x$ps_code, x$dn_code, x$nm_code, x$NDZ_sanemsanas_datums, x$start), ]
  x <- x[ , c("period", "ps_code", "dn_code", "nm_code", "dienas")]
  
  x <- x %>%
    group_by(period, ps_code, dn_code, nm_code) %>%
    summarise(
      dienas = sum(dienas, na.rm = TRUE)
    ) %>%
    arrange(ps_code)

  if ((nrow(x[duplicated(x[c("ps_code", "nm_code")]), ]) == 0) && (sum(x$dienas < 0) == 0)) {
    setwd(paste0(path, "data\\intermediate_tables\\"))
    load("temp_NDZ.RData")
    
    temp_NDZ <-
      rbind(temp_NDZ, x[, c("period",
                            "ps_code",
                            "dn_code",
                            "nm_code",
                            "dienas")]) 
    save(temp_NDZ, file = "temp_NDZ.RData")
    
    temp_rows <- readRDS("temp_rows.rds")
    temp_rows <- append(temp_rows, nrow(x))
    saveRDS(temp_rows, file = "temp_rows.RDS")
    rm(temp_NDZ, x, temp_rows)
  } else {
    stop(cat("Tabulā ir dubultnieki vai negatīvas dienas. o ir", o, ".\n"))
  }
}

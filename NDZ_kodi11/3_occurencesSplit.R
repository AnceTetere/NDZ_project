setwd(paste0(path, "data\\intermediate_tables\\"))
check_number <- nrow(NDZ)
occurance_number <- 1
tabs_NDZ <- vector("character")

while (check_number > 0) {
  
  x <- NDZ %>%
    group_by(ps_code, dn_code, nm_code) %>%
    filter(n() == occurance_number) %>%
    ungroup() %>%
    arrange(ps_code, NDZ_sanemsanas_datums)
  
  tab_name <- paste0("NDZ_", occurance_number)
  
  if(nrow(x) > 0) {
    assign(tab_name, x)
    check_number <- check_number - nrow(x)
    tabs_NDZ <- append(tabs_NDZ, tab_name)
    
    save(list = tab_name, file = paste0(tab_name, ".RData"))
    rm(list = tab_name) 
    rm(tab_name, x) #izdzēšu, jo saglabāju
    }
    
  occurance_number <- occurance_number + 1
}
saveRDS(tabs_NDZ, file = "tabs_NDZ.rds")
rm(NDZ, check_number, occurance_number)

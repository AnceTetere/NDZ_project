occurencesSplit <- function(NDZ) {

setwd(paste0(path, "data\\intermediate_tables\\"))
check_number <- nrow(NDZ)
occurance_number <- 1
tabs_ndz <- vector("character")

while (check_number > 0) {
  
  x <- NDZ %>%
    group_by(PS_code, DN_code, NM_code) %>%
    filter(n() == occurance_number) %>%
    ungroup() %>%
    arrange(PS_code, NDZ_sanemsanas_datums)
  
  tab_name <- paste0("NDZ_", occurance_number)
  
  if(nrow(x) > 0) {
    assign(tab_name, x)
    check_number <- check_number - nrow(x)
    tabs_ndz <- append(tabs_ndz, tab_name)
    
    save(list = tab_name, file = paste0(tab_name, ".RData"))
    rm(list = tab_name) 
    rm(tab_name, x) #izdzēšu, jo saglabāju
    }
    
  occurance_number <- occurance_number + 1
}

saveRDS(tabs_ndz, file = "tabs_ndz.rds")
rm(NDZ, check_number, occurance_number)

return(tabs_ndz)

}

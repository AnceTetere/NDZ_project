occurencesSplit <- function(NDZ) {

check_number <- nrow(NDZ)
occurance_number <- 1

tabs_zdn <- vector("character")
NDZ_list <- list()

while (check_number > 0) {
  
  x <- NDZ %>%
    group_by(PS_code, DN_code, NM_code) %>%
    dplyr::filter(n() == occurance_number) %>%
    ungroup() %>%
    arrange(PS_code, NDZ_sanemsanas_datums)
  
  tab_name <- paste0("NDZ_", occurance_number)
  
  if(nrow(x) > 0) {
    assign(tab_name, x, envir = environment())
    check_number <- check_number - nrow(x)
    
    tabs_zdn <- append(tabs_zdn, tab_name)
    NDZ_list[[length(tabs_zdn)]] <- get(tab_name)
  rm(tab_name, x)
    }
    
  occurance_number <- occurance_number + 1
}

rm(check_number, occurance_number)

return(list(tabs_zdn = tabs_zdn, NDZ_list = NDZ_list))
}

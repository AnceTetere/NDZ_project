occurencesSplit <- function(NDZ) {

setwd(paste0(path, "data\\intermediate_tables\\"))
check_number <- nrow(NDZ)
occurance_number <- 1

tabs_ndz <- vector("character")
NDZ_list <- list()

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
    NDZ_list[[length(tabs_ndz)]] <- get(tab_name) 
    rm(tab_name, x)
    }
    
  occurance_number <- occurance_number + 1
}

rm(check_number, occurance_number)

result <- list(tabs_ndz = tabs_ndz, NDZ_list = NDZ_list)
return(result)
}

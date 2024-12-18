buildingMM <- function(x, mp, dd_skaits) { 

  if (sum(x$dd > dd_skaits) == 0) {
    tName <- paste0("MM", mp, "_", year, MM)
    assign(tName, x, envir = environment())
    save(list = tName, file = paste0("data/starptabulas/buildingMM/", tName, ".RData"))
    rm(list = tName, tName, x, dd_skaits)
  } else {
    stop()}
  
  #TODO: Te, kad viss galā, izstrādā return funkciju.
}

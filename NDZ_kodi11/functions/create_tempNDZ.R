tempNDZ <- function(){
  #Izveido tukšo tabulu, kurā būvēsi gala tabulu
  temp_NDZ <- data.frame(
    period = character(), 
    PS_code = character(),
    DN_code = character(),
    NM_code = character(),
    dienas = integer()
  )
  #Šī ir visai smaga apstrāde, tāpēc izstrādes tabula tiek saglabāta
  save(temp_NDZ, file = paste0("data/starptabulas/", year, "/temp_NDZ.RData"))
  rm(temp_NDZ)
  
  #Izveido tukšu vektoru, kas skaitīs rindas, kas ievietotas temp_NDZ tabulā
  temp_rows <- vector()
  saveRDS(temp_rows, file = paste0("data/starptabulas/", year, "/temp_rows.RDS"))
  rm(temp_rows)
}

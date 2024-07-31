tempNDZ <- function(){
  #Izveido tukšo tabulu, kurā būvēsi gala tabulu
  temp_NDZ <- data.frame(
    period = character(), 
    PS_code = character(),
    DN_code = character(),
    NM_code = character(),
    dienas = integer()
  )
  
  #Izveido tukšu vektoru, kas skaitīs rindas, kas ievietotas temp_NDZ tabulā
  temp_rows <- vector()
  
  return(list(temp_NDZ = temp_NDZ, temp_rows = temp_rows))
}

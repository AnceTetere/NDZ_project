menesa_dienu_skaits <- function(year, month) {
  
  #1 Pārbauda saņemto mēnesi
  month <- as.numeric(month)
  if (month < 1 || month > 12) {
    stop("Neatbilstošs mēnesis!")
  }  
    
  #2 Izveido R objektu, kas nes nākošā mēneša pirmo dienu.
  year <- as.numeric(year)
  if(month == 12) {
    nakamais_menesis <- as.Date(paste(year + 1, 1, 1, sep = "-"))
  } else {
    nakamais_menesis <- as.Date(paste(year, month + 1, 1, sep = "-"))
  }  
    
  #3 Sarēkina dienu skaitu
  dienu_skaits <- as.integer(format(nakamais_menesis - 1, "%d"))
  rm(month, year, nakamais_menesis)
  
  return(dienu_skaits)
}

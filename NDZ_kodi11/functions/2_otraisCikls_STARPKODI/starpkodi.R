#FOR TESTS: y_2plus = y2; n = 2; rm(y2)

starpkodi <- function(y_2plus, n) { 
  y_2plus <- y_2plus %>% arrange(PS_code, dnperk, NM_code)
  
  prev <- as.Date(paste0(substr(y_2plus$period[1], 1, 4), "-", substr(y_2plus$period[1], 5, 6), "-01")) - 1
  z <- data.frame()
  
  #options(warn = 1)
  for(v in seq(1, nrow(y_2plus), by = n)) {

    t <- subTable(y_2plus, n, v)

    if(!(all(t$PS_code == t$PS_code[1] & t$NM_code == t$NM_code[1]))) {
      stop("Pseidokodu nesakritība! Tabula: y_2plus; rinda:", v)}

    z <- switch(
      as.character(n),
      "2" = dubultkodi(y_2plus, t, prev, v, z),
      "3" = tripletkodi(y_2plus, t, prev, v, z),
      default = stop("Starpkodi neatpazīst atšķirīgo kodu tabulu y_2plus.")
    )
    print(v)
  }
  rm(prev, t, y_2plus)
  return(z) 
}

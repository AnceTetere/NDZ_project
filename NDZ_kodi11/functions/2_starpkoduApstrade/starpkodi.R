starpkodi <- function(y_2plus, n) { #y_2plus: apstrādā tabulas, kur ir divi atšķirīgi kodi vai vairāk; n: skaits, cik šo atšķirīgo kodu ir
  setwd(paste0(path, "data\\originals\\", year))
  y_2plus <- y_2plus[order(y_2plus$PS_code, y_2plus$DN_code, y_2plus$NM_code), ]
  
  prev <- as.Date(paste0(substr(y_2plus$period[1], 1, 4), "-", substr(y_2plus$period[1], 5, 6), "-01")) - 1
  z <- data.frame()
  
  for(v in seq(1, nrow(y_2plus), by = n)) {
    
    # izveido apakštabulu no oriģinālajiem datiem, pārrēķinam
    t <- subTable(y_2plus, n, v)
    
    if(!(all(t$PS_code == t$PS_code[1]) && all(t$NM_code == t$NM_code[1]))) {
      stop("PS_code nesakritība! Tabula: y_2plus; rinda:", v)
    }
    
    z <- switch(
      as.character(n),
      "2" = dubultkodi(y_2plus, t, prev, v, z),
      "3" = tripletkodi(y_2plus, t, prev, v, z),
      default = stop("Starpkodi neatpazīst atšķirīgo kodu tabulu y_2plus.")
    )
  }
 
  rm(prev, t, y_2plus)
  return(z) 
}

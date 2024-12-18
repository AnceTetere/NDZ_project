starpkodi <- function(y_2plus, n) { 
  y_2plus <- arrange(y_2plus, PS_code, DN_code, NM_code)
  
  prev <- as.Date(paste0(substr(y_2plus$period[1], 1, 4), "-", substr(y_2plus$period[1], 5, 6), "-01")) - 1
  z <- data.frame()
  
  for(v in seq(1, nrow(y_2plus), by = n)) {
    t <- subTable(y_2plus, n, v)
    if(!(all(t$PS_code == t$PS_code[1] & t$NM_code == t$NM_code[1]))) {
      stop()}

    z <- switch(
      as.character(n),
      "2" = dubultkodi(y_2plus, t, prev, v, z),
      "3" = tripletkodi(y_2plus, t, prev, v, z),
      default = stop()
    )
  }
  rm(prev, t, y_2plus)
  return(z) 
}

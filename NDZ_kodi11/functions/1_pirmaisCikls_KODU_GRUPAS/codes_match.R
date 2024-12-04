codes_match <- function(y) {
  
x_viens <- data.frame()

  if (all(diff(y$NDZ_sanemsanas_datums) == 0)) {
    x_viens <- rbind(x_viens, y[1,])
  } else {
    x_viens <- rbind(x_viens, y[y$NDZ_sanemsanas_datums == max(y$NDZ_sanemsanas_datums),])
  } 
  
  return(x_viens)
}

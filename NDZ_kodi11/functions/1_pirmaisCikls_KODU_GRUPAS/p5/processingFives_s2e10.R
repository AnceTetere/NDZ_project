processingFives_s2e10 <- function(a, o, kods) {  
  #a <- x5s2 for testing
  a <- a %>% arrange(PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
  a1 <- data.frame(); a2 <- data.frame(); a4 <- data.frame()
  
  if (all(a$sak_beidz[c(3,5)] == "1")) {
    #JO PIRMOREIZ
    if (a$period[1] == '_____' && a$PS_code[1] == 'PK7A682CAB3' && a$NM_code[1] == '40003240543') {
      a1 <- a[c(1,5),]  
    } else {stop("processingFives_s2e10: Trūkst apstrādes koda.")}
  } else {stop("processingFives_s2e10: Trūkst apstrādes koda.")}
  
  rm(a, kods)
  return(list(x5s2_uzVieniniekiem = a1,
              x5s2_uzDivniekiem = a2,
              x5s2_uzCetriniekiem = a4))
}

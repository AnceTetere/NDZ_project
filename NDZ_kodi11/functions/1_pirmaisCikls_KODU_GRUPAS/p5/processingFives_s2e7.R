processingFives_s2e7 <- function(a, kods) {  
  #a <- a for testing
  a <- a %>% arrange(PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
  a1 <- data.frame(); a2 <- data.frame(); a4 <- data.frame()
  
  if (all(x5s2$sak_beidz == c("2", "1", "2", "1", "2"))) {
    if ((x5s2$PS_code[1] == '_______' && x5s2$NM_code[1] == '_________') ||
        (x5s2$PS_code[1] == '________' && x5s2$NM_code[1] == '________')) {
      a2 <- rbind(a2, x5s2[c(2,1,4,5),])
    } else {stop("processingFivess2e7: Trūkst apstrādes koda.")}
    if (kods %in% c("40", "50", "53")) {ZERO_plus(x5s2 %>% slice(5))}
  } else {stop("processingFivess2e7: Trūkst apstrādes koda.")}
  
  rm(a, kods)
  return(list(x5s2_uzVieniniekiem = a1,
              x5s2_uzDivniekiem = a2,
              x5s2_uzCetriniekiem = a4))
}

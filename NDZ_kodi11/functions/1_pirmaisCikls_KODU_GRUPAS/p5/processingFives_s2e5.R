processingFives_s2e5 <- function(a, kods) {  
  #a <- a for testing
  a <- a %>% arrange(PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
  a1 <- data.frame(); a2 <- data.frame(); a4 <- data.frame()
  
  if (all(x5s2$sak_beidz[4:5] == "1")) {
    a1 <- rbind(a1, x5s2[c(1,5), ])
  } else if (all(x5s2$sak_beidz[3:4] == "1")) {
    if (x5s2$PS_code[1] == '__________' && x5s2$NM_code[1] == '__________') {
      a1 <- rbind(a1, x5s2[1, ])
      a2 <- rbind(a2, x5s2[4:5, ])
    } else {stop("processingFives_s2e5: Trūkst apstrādes koda.")}
    if (kods %in% c("40", "50", "53")) {ZERO_plus(x5s2 %>% slice(5))}
  } else {stop("processingFives_s2e5: Trūkst apstrādes koda.")}
  
  rm(a, kods)
  return(list(x5s2_uzVieniniekiem = a1,
              x5s2_uzDivniekiem = a2,
              x5s2_uzCetriniekiem = a4))
}


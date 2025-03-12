processingFives_s2e5 <- function(a, kods) {  
  #a <- x5s2 for testing
  a <- a %>% arrange(PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
  a1 <- data.frame(); a2 <- data.frame(); a4 <- data.frame()
  
  if (all(a$sak_beidz[4:5] == "1")) {
    a1 <- rbind(a1, a[c(1,5), ])
  } else if (all(a$sak_beidz[3:4] == "1")) {
    if (a$PS_code[1] == '_____' && a$NM_code[1] == '______') {
      a1 <- rbind(a1, a[1, ])
      a2 <- rbind(a2, a[4:5, ])
    } else {stop("processingFives_s2e5: Trūkst apstrādes koda.")}
    if (kods %in% c("40", "50", "53")) {ZERO_plus(a %>% slice(5))}
  } else {stop("processingFives_s2e5: Trūkst apstrādes koda.")}
  
  rm(a, kods)
  return(list(a_uzVieniniekiem = a1,
              a_uzDivniekiem = a2,
              a_uzCetriniekiem = a4))
}

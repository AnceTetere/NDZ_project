processingFives_s2e9 <- function(a, kods) {  
  #a <- a for testing
  a <- a %>% arrange(PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
  a1 <- data.frame(); a2 <- data.frame(); a4 <- data.frame()
  
  if (all(a$sak_beidz == c("2", "2", "1", "2", "1"))) {
    if (a$PS_code[1] == '________' && a$NM_code[1] == '________1') {
      a1 <- rbind(a1, a[1,])  
      a2 <- rbind(a2, a[c(3,2,5,4),])
    } else {stop("processingFives_s2e9: Trūkst apstrādes koda.")}
  } else if (all(a$sak_beidz == c("2", "1", "2", "1", "2"))) {
    if (a$PS_code[1] == '________' && a$NM_code[1] == '________' && a$period[1] == "________") {
      a1 <- rbind(a1, a[1,])  
      a2 <- rbind(a2, a[2:5,])
    } else {stop("processingFives_s2e9: Trūkst apstrādes koda.")}
    if (kods %in% c("40", "50", "53")) {ZERO_plus(a %>% slice(5))}
  } else if (all(a$sak_beidz == c("2", "1", "2", "2", "1"))) {
    if (a$PS_code[1] == '________' && a$NM_code[1] == '________' && a$period[1] == "________") {
      a <- a %>% slice(1,2,3,5,4)
      a1 <- rbind(a1, a[1,])  
      a2 <- rbind(a2, a[2:5,])
      if (kods %in% c("40", "50", "53")) {ZERO_plus(a %>% slice(5))}
    } else {stop("processingFives_s2e9: Trūkst apstrādes koda.")}
  } else {stop("processingFives_s2e9: Trūkst apstrādes koda.")}
  
  rm(a, kods)
  return(list(x5s2_uzVieniniekiem = a1,
              x5s2_uzDivniekiem = a2,
              x5s2_uzCetriniekiem = a4))
}

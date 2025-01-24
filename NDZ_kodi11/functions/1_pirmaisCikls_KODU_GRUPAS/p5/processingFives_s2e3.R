processingFives_s2e3 <- function(a, kods) {  
  #a <- a for testing
  a <- a %>% arrange(PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
  a1 <- data.frame(); a2 <- data.frame(); a4 <- data.frame()
  
  if (all(a$sak_beidz[c(2,4)] == "1") && a$zinkod[3] == "26") {
    a1 <- rbind(a1, a[1, ])
    a4 <- rbind(a4, a[-1, ])
  } else if (all(a$sak_beidz[c(1,4)] == "1")) {
    a2 <- rbind(a2, a[-3, ])
    if (kods %in% c("40", "50", "53")) {ZERO_plus(a %>% slice(5)); ZERO_minus(a %>% slice(1))}
  } else if (all(a$sak_beidz[c(2,4)] == "1")) {
    if (a$period[1] == "________" && a$PS_code[1] == '________' && a$NM_code[1] == '________') {
      a1 <- rbind(a1, a[1, ])
      a2 <- rbind(a2, a[-1, ])
    } else {stop("processingFives_s2e3: Trūkst apstrādes koda.")}
    if (kods %in% c("40", "50", "53")) {ZERO_plus(a %>% slice(5))}
  } else {stop("processingFives_s2e3: Trūkst apstrādes koda.")}
  
  rm(a, kods)
  return(list(x5s2_uzVieniniekiem = a1,
              x5s2_uzDivniekiem = a2,
              x5s2_uzCetriniekiem = a4))
}

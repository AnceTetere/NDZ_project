processingFives_s2e4 <- function(a, o, kods) {  
  a <- a %>% arrange(PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
  a1 <- data.frame(); a2 <- data.frame(); a4 <- data.frame()
  
  if (all(a$sak_beidz[c(1,4)] == "1")) {
    a4 <- rbind(a4, a[c(1,3:5), ])
    if (kods %in% c("40", "50", "53")) {ZERO_plus(a %>% slice(5))}
  } else if (all(a$sak_beidz[c(2,4)] == "1")) {
    a1 <- a[1, ]; a4 <- a[2:5, ]
    if (kods %in% c("40", "50", "53") && o == "5") {ZERO_plus(a %>% slice(5))}
  } else {stop("processingFives_s2: Trūkst apstrādes koda.")}
  
  rm(a, kods)
  return(list(x5s2_uzVieniniekiem = a1,
              x5s2_uzDivniekiem = a2,
              x5s2_uzCetriniekiem = a4))
}

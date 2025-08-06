processingFives_s2e11 <- function(a, o, kods) {  
  #a <- x5s2 for testing
  a <- a %>% arrange(PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
  a1 <- data.frame(); a2 <- data.frame(); a4 <- data.frame()
  
  if (all(a$sak_beidz[c(2,5)] == "1")) {
    if (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') {
        a1 <- a[1,]; a4 <- a[c(2,3,5,4),]
        if (kods %in% c("40", "50", "53") && o == "5") {ZERO_plus(a %>% slice(4))}
    } else {stop("processingFives_s2e11: Trūkst apstrādes koda.")}
  } else {stop("processingFives_s2e11: Trūkst apstrādes koda.")}
  
  rm(a, o, kods)
  return(list(x5s2_uzVieniniekiem = a1,
              x5s2_uzDivniekiem = a2,
              x5s2_uzCetriniekiem = a4))
}

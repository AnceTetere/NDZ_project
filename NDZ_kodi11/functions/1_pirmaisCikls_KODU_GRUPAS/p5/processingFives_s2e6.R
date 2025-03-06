processingFives_s2e6 <- function(a, o, kods) {  
  a <- a %>% arrange(PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
  a1 <- data.frame(); a2 <- data.frame(); a4 <- data.frame()
  
  if (all(a$sak_beidz[3:4] == "1")) {
           a1 <- rbind(a1, a[1,])
           a2 <- rbind(a2, a[c(3,2,4,5),])
           if (kods %in% c("40", "50", "53") && o == "5") {ZERO_plus(a %>% slice(5))}
  } else if (all(a$sak_beidz[c(2,4)] == "1")) {
           if ((a$PS_code[1] == '_____' && a$NM_code[1] == '_____') ||
               (a$PS_code[1] == '_____' && a$NM_code[1] == '_____') ||
               (a$period[1] == '_____' && a$PS_code[1] == '_____' && a$NM_code[1] == '_____') ||
               (a$period[1] == '_____' && a$PS_code[1] == '_____' && a$NM_code[1] == '_____') ||
               (a$period[1] == '_____' && a$PS_code[1] == '_____' && a$NM_code[1] == '_____')) {
             a1 <- rbind(a1, a[1,])
             a2 <- rbind(a2, a[2:5,])
           } else {stop("processingFives_s2e6: Trūkst apstrādes koda.")}
    if (kods %in% c("40", "50", "53") && o == "5") {ZERO_plus(a %>% slice(5))}
  } else if (all(a$sak_beidz[c(3,5)] == "1")) {
           if ((a$PS_code[1] == '_____' && a$NM_code[1] == '_____') ||
               (a$PS_code[1] == '_____' && a$NM_code[1] == '_____')) {
                   a1 <- rbind(a1, a[c(1,5),])
                   a2 <- rbind(a2, a[3:4,])
           } else {stop("processingFives_s2e6: Trūkst apstrādes koda.")}
  } else if (all(a$sak_beidz == c("2", "1", "2", "2", "1"))) {
    if (a$PS_code[1] == '_____' && a$NM_code[1] == '_____') {
      a1 <- rbind(a1, a[c(1,5),])
      a2 <- rbind(a2, a[c(2,4),])
    } else {stop("processingFives_s2e6: Trūkst apstrādes koda.")}
  } else {stop("processingFives_s2e6: Trūkst apstrādes koda.")}
  
  rm(a, o, kods)
  return(list(x5s2_uzVieniniekiem = a1,
              x5s2_uzDivniekiem = a2,
              x5s2_uzCetriniekiem = a4))
  }

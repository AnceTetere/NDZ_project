processingFives_s2e6 <- function(a, kods) {  
  #a <- a for testing
  a <- a %>% arrange(PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
  a1 <- data.frame(); a2 <- data.frame(); a4 <- data.frame()
  
  if (all(a$sak_beidz[3:4] == "1")) {
    a1 <- rbind(a1, a[1,])
    a2 <- rbind(a2, a[c(3,2,4,5),])
    if (kods %in% c("40", "50", "53")) {ZERO_plus(a %>% slice(5))}
  } else if (all(a$sak_beidz == c("2", "1", "2", "1", "2"))) {
    if ((a$PS_code[1] == '___________' && a$NM_code[1] == '___________') ||
        (a$PS_code[1] == '___________' && a$NM_code[1] == '___________') ||
        (a$period[1] == '___________' && a$PS_code[1] == '___________' && a$NM_code[1] == '___________') ||
        (a$period[1] == '___________' && a$PS_code[1] == '___________' && a$NM_code[1] == '___________')) {
      a1 <- rbind(a1, a[1,])
      a2 <- rbind(a2, a[2:5,])
    } else {stop("processingFives_s2e6: Trūkst apstrādes koda.")}
    if (kods %in% c("40", "50", "53")) {ZERO_plus(a %>% slice(5))}
  } else if (all(a$sak_beidz == c("2", "2", "1", "2", "1"))) {
    if (a$PS_code[1] == '___________' && a$NM_code[1] == '___________') {
      a1 <- rbind(a1, a[c(1,5),])
      a2 <- rbind(a2, a[3:4,])
    } else {stop("processingFives_s2e6: Trūkst apstrādes koda.")}
  } else if (all(a$sak_beidz == c("2", "1", "2", "2", "1"))) {
    if (a$PS_code[1] == '___________' && a$NM_code[1] == '___________') {
      a1 <- rbind(a1, a[c(1,5),])
      a2 <- rbind(a2, a[c(2,4),])
    } else {stop("processingFives_s2e6: Trūkst apstrādes koda.")}
  } else {stop("processingFives_s2e6: Trūkst apstrādes koda.")}
  
  rm(a, kods)
  return(list(x5s2_uzVieniniekiem = a1,
              x5s2_uzDivniekiem = a2,
              x5s2_uzCetriniekiem = a4))
  }

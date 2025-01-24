processingFives_s3e3 <- function(a, kods) {
  a1 <- data.frame(); a2 <- data.frame(); a4 <- data.frame()
  
  if (all(a$sak_beidz[c(1, 3, 5)] == "1")) {
    a1 <- rbind(a1, a[5, ])
    a2 <- rbind(a2, a[1:4, ])
    if (kods %in% c("40", "50", "53")) {ZERO_minus(a %>% slice(1))}
  } else if (all(a$sak_beidz[c(1, 3, 4)] == "1")) {
    if (a$PS_code[1] == '__________' && a$NM_code[1] == '__________') {
      a2 <- rbind(a2, a[c(1:3,5), ])
    } else if (a$PS_code[1] == '__________' && a$NM_code[1] == '__________') {
      a2 <- rbind(a2, a[c(1:3,5), ])
    } else {stop("processingFives_s3e3 iztrūkst apstrādes koda")}
    if (kods %in% c("40", "50", "53")) {ZERO_minus(a %>% slice(1)); ZERO_plus(a %>% slice(5))}
  } else {stop("processingFives_s3e3 iztrūkst apstrādes koda")}
  
  
  rm(a)
  return(list(x5s3_uzVieniniekiem = a1, 
              x5s3_uzDivniekiem = a2,
              x5s3_uzCetriniekiem = a4)) 
}

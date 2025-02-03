processingFives_s3e5 <- function(a, kods) {
  a1 <- data.frame(); a2 <- data.frame(); a4 <- data.frame()
  
  if (all(a$sak_beidz[1:3] == "1")) {
        a2 <- rbind(a2, a[c(3,5), ])
        if (kods %in% c("40", "50", "53")) {ZERO_plus(a %>% slice(5)); ZERO_minus(a %>% slice(1))}
  } else {stop("processingFives_s3e5 iztrūkst apstrādes koda")}
  
  rm(a)
  return(list(x5s3_uzVieniniekiem = a1, 
              x5s3_uzDivniekiem = a2,
              x5s3_uzCetriniekiem = a4)) 
}

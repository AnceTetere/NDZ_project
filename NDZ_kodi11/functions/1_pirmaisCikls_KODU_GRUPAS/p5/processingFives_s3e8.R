processingFives_s3e8 <- function(a, o, kods) {
  a1 <- data.frame(); a2 <- data.frame(); a4 <- data.frame()
  #a <- a for testing
  
  if (all(a$sak_beidz[c(1,4,5)] == "1")){
    a1 <- rbind(a1, a[5, ])
    a2 <- rbind(a2, a[c(1, 3), ])
    if (kods %in% c("40", "50", "53") && o == "5") {ZERO_minus(a %>% slice(1))}
  } else {stop("processingFives_s3 iztrūkst apstrādes koda")}
  
  
  rm(a)
  return(list(x5s3_uzVieniniekiem = a1, 
              x5s3_uzDivniekiem = a2,
              x5s3_uzCetriniekiem = a4)) 
}

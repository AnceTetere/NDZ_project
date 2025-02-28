processingFives_s3e6 <- function(a, o, kods) {
  a1 <- data.frame(); a2 <- data.frame(); a4 <- data.frame()
  #a <- x5s3 for testing
  
  if (all(a$sak_beidz[3:5] == "1")) {
    a1 <- rbind(a1, a[c(3,5), ])
  } else {stop("processingFives_s3 iztrūkst apstrādes koda")}
  
  rm(a)
  return(list(x5s3_uzVieniniekiem = a1, 
              x5s3_uzDivniekiem = a2,
              x5s3_uzCetriniekiem = a4)) 
}

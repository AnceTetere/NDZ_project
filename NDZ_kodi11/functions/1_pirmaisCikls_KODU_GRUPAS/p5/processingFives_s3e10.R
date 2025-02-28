processingFives_s3e10 <- function(a, o, kods) {
  a1 <- data.frame(); a2 <- data.frame(); a4 <- data.frame()
  
  if (all(a$sak_beidz[3:5] == "1")) {
    a1 <- rbind(a1, a[c(1, 5), ])
  } else {stop("processingFives_s3: Iztrūkst apstrādes koda!\n")}  

  
  rm(a)
  return(list(x5s3_uzVieniniekiem = a1, 
              x5s3_uzDivniekiem = a2,
              x5s3_uzCetriniekiem = a4)) 
}

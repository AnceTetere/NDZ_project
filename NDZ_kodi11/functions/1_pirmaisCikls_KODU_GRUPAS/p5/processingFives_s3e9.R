processingFives_s3e9 <- function(a, o, kods) {
  a1 <- data.frame(); a2 <- data.frame(); a4 <- data.frame()
  
  if (!("26" %in% a$zinkod[a$sak_beidz == "2"])) {
    a1 <- rbind(a1, a[5, ])
    a2 <- rbind(a2, a[c(1, 3), ])
  } else {stop("processingFives_s3: Iztrūkst apstrādes koda!\n")}  

  
  rm(a)
  return(list(x5s3_uzVieniniekiem = a1, 
              x5s3_uzDivniekiem = a2,
              x5s3_uzCetriniekiem = a4)) 
}

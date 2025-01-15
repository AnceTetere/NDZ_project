processingFives_s3e1 <- function(a) {
  a1 <- data.frame(); a2 <- data.frame(); a4 <- data.frame()

if (all(a$sak_beidz == c("2", "1", "1", "2", "1"))) {
  a1 <- rbind(a1, a[5,])
  a2 <- rbind(a2, a[c(2,1,3,4),])
} else if (all(a$sak_beidz == c("2", "1", "2", "1", "1"))) {
  if ((a$PS_code[1] == '______________' && a$NM_code[1] == '______________') ||
      (a$PS_code[1] == '______________' && a$NM_code[1] == '______________')) {
    a1 <- rbind(a1, a[5,])
    a2 <- rbind(a2, a[2:3,])
  } else {stop("processingFives_s3e1: Iztrūkst apstrādes koda!\n")}
} else {stop("processingFives_s3e1: Iztrūkst apstrādes koda!\n")}
  
  rm(a)
  return(list(x5s3_uzVieniniekiem = a1, 
              x5s3_uzDivniekiem = a2,
              x5s3_uzCetriniekiem = a4)) 
  }

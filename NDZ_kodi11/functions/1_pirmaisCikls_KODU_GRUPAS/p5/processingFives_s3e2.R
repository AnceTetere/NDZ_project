processingFives_s3e2 <- function(a) {
  a1 <- data.frame(); a2 <- data.frame(); a4 <- data.frame()
  
  if(all(a$sak_beidz[c(2,4,5)] == "1")) {
    a1 <- rbind(a1, a[c(1, 5), ])
    a2 <- rbind(a2, a[2:3, ])
  } else if (all(a$sak_beidz[c(2,3,5)] == "1")) {
    a1 <- rbind(a1, a[c(1, 5), ])
    a2 <- rbind(a2, a[3:4, ])
  } else if (all(a$sak_beidz[2:4] == "1")) {
    a1 <- rbind(a1, a[1, ])
    a2 <- rbind(a2, a[4:5, ])
  } else if (all(a$sak_beidz[c(1,3,5)] == "1")) {
    a1 <- rbind(a1, a[5, ])
    a2 <- rbind(a2, a[1:4, ])
  } else if (all(a$sak_beidz[c(1,3,4)] == "1")) {
    a4 <- rbind(a4, a[c(1,2,4,5),])
  } else if (all(a$sak_beidz[c(1,2,4)] == "1")) {
    if (a$PS_code[1] == '________' && a$NM_code[1] == '________') {
      a[1, 'sak_beidz'] <- "2"
      a[1, 'zinkod'] <- "50"
      a1 <- rbind(a1, a[1,])
      a4 <- rbind(a4, a[2:5,])
    } else {stop("processingFives_s3e2 iztrūkst apstrādes koda")}
  } else {stop("processingFives_s3e2 iztrūkst apstrādes koda")} 

    rm(a)
    return(list(x5s3_uzVieniniekiem = a1, 
                x5s3_uzDivniekiem = a2,
                x5s3_uzCetriniekiem = a4))
}

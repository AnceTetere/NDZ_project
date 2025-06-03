processingFives_s3e2 <- function(a, o, kods) {
  a1 <- data.frame(); a2 <- data.frame(); a4 <- data.frame()
  #a <- x5s3 testiem
  
  if (all(a$sak_beidz[c(2,4,5)] == "1")) {
    a1 <- a[c(1,5), ]; a2 <- a[2:3, ]
  } else if (all(a$sak_beidz[c(2,3,5)] == "1")) {
    a1 <- a[c(1,5), ]; a2 <- a[3:4, ]
  } else if (all(a$sak_beidz[3:5] == "1")) {
    if (a$period[1] == "______" && a$PS_code == "___________" && a$NM_code == "___________") {
      a1 <- a[c(2,5), ]
    } else {stop("processingFives_s3e2 iztrūkst apstrādes koda")} 
  } else if (all(a$sak_beidz[2:4] == "1")) {
    a1 <- a[1, ]; a2 <- a[4:5, ]
  } else if (all(a$sak_beidz[c(1,3,5)] == "1")) {
    a1 <- a[5, ]; a2 <- a[1:4, ]
    if (kods %in% c("40", "50", "53") && o == "5") {ZERO_minus(a %>% slice(1))}
  } else if (all(a$sak_beidz[c(1,3,4)] == "1")) {
    a4 <- a[c(1,2,4,5),]
    if (kods %in% c("40", "50", "53") && o == "5") {ZERO_minus(a %>% slice(1)); ZERO_plus(a %>% slice(5))}
  } else if (all(a$sak_beidz[c(1,2,4)] == "1")) {
    if (a$PS_code == "___________" && a$NM_code == "___________") {
      a[1, 'sak_beidz'] <- "2"
      a[1, 'zinkod'] <- "50"
      a1 <- rbind(a1, a[1,])
      a4 <- rbind(a4, a[2:5,])
      if (kods %in% c("40", "50", "53") && o == "5") {ZERO_minus(a %>% slice(1)); ZERO_plus(a %>% slice(5))}
    } else if ((a$period[1] == "______" && a$PS_code == "___________" && a$NM_code == "___________") ||
               (a$period[1] == "______" && a$PS_code == "___________" && a$NM_code == "___________") ||
               (a$period[1] == "______" && a$PS_code == "___________" && a$NM_code == "___________")) {
      a4 <- rbind(a4, a[2:5,])
    } else {stop("processingFives_s3e2 iztrūkst apstrādes koda")}
  } else {stop("processingFives_s3e2 iztrūkst apstrādes koda")} 

    rm(a, kods)
    return(list(x5s3_uzVieniniekiem = a1, 
                x5s3_uzDivniekiem = a2,
                x5s3_uzCetriniekiem = a4))
}

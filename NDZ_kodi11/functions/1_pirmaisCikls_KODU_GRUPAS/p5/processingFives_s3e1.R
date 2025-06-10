processingFives_s3e1 <- function(a, o, kods) {
  a1 <- data.frame(); a2 <- data.frame(); a4 <- data.frame()
  #a <- x5s3

  if (all(a$sak_beidz[c(2,4,5)] == "1")) {
        if ((a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') ||
            (a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________')) {
          a1 <- rbind(a1, a[5,])
          a2 <- rbind(a2, a[2:3,])
          if (kods %in% c("40", "50", "53") && o == "5") {ZERO_minus(a %>% slice(3))}
        } else if (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') {
          a1 <- rbind(a1, a[c(1,5),])
          a2 <- rbind(a2, a[2:3,])
        } else {stop("processingFives_s3e1: Iztrūkst apstrādes koda!\n")}
} else if (all(a$sak_beidz[c(1,4,5)] == "1")) {
          if ((a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') ||
              (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________')) {
              a1 <- a[5,]; a2 <- a[c(1,3),]
          } else {stop("processingFives_s3e1: Iztrūkst apstrādes koda!\n")}
} else if (all(a$sak_beidz[c(2,3,5)] == "1")) {
          if (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') {
            a1 <- a[c(1,5),]; a2 <- a[3:4,]
          } else if ((a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________')  ||
              (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________')  ||
              (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________')  ||
              (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________')  ||
              (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________')  ||
              (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________')  ||
              (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________'))  {
            a <- a[c(2,1,3,4,5),]; a1 <- a[5,]; a2 <- a[1:4,]
            if (kods %in% c("40", "50", "53") && o == "5") {ZERO_minus(a %>% slice(1))}
          } else {stop("processingFives_s3e1: Iztrūkst apstrādes koda!\n")}
} else if (all(a$sak_beidz[c(1,3,4)] == "1")) {
  if (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________')  {
    a2 <- a[c(1,2,4,5),]
    if (kods %in% c("40", "50", "53") && o == "5") {ZERO_minus(a %>% slice(1)); ZERO_plus(a %>% slice(5))}
  } else {stop("processingFives_s3e1: Iztrūkst apstrādes koda!\n")}
} else {stop("processingFives_s3e1: Iztrūkst apstrādes koda!\n")}
  
  rm(a)
  return(list(x5s3_uzVieniniekiem = a1, 
              x5s3_uzDivniekiem = a2,
              x5s3_uzCetriniekiem = a4)) 
  }

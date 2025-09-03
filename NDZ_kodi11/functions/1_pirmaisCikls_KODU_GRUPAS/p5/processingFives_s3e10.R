processingFives_s3e10 <- function(a, o, kods) {
  a1 <- data.frame(); a2 <- data.frame(); a4 <- data.frame()
  #a <- x5s3 for testing
  
  if (all(a$sak_beidz[3:5] == "1")) {
         #JO PIRMOREIZ
         if ((a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') ||
             (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') ||
             (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________')) {
              a1 <- a[c(1,5),]
        } else {stop("processingFives_s3e10: Iztrūkst apstrādes koda!\n")}
  } else if (all(a$sak_beidz[c(2,3,5)] == "1")) {
        if ((a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') ||
            (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') ||
            (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________')) {
             #JO PIRMOREIZ
             a1 <- a[5, ]; a4 <- a[c(2,1,3,4),]
        } else {stop("processingFives_s3e10: Iztrūkst apstrādes koda!\n")}
  } else if (all(a$sak_beidz[c(1,3,4)] == "1")) {
          #Te datumi ir samainījušies vietām.
           if (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') {
              #JO PIRMOREIZ
               a1 <- a[4, ]; a4 <- a[c(1,2,3,5),]
           } else {stop("processingFives_s3e10: Iztrūkst apstrādes koda!\n")}  
  } else {stop("processingFives_s3e10: Iztrūkst apstrādes koda!\n")}  

  
  rm(a)
  return(list(x5s3_uzVieniniekiem = a1, 
              x5s3_uzDivniekiem = a2,
              x5s3_uzCetriniekiem = a4)) 
}

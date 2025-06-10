processingFives_s3e12 <- function(a, o, kods) {
  a1 <- data.frame(); a2 <- data.frame(); a4 <- data.frame()

  a <- a %>% dplyr::arrange(NDZ_sanemsanas_datums)
  
  if (all(a$sak_beidz[c(2,4,5)] == "1")) {
          if((a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') ||
             (a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') ||
             (a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') ||
             (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') ||
             (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________')) {
                a1 <- a[5,]; a2 <- a[c(2,1,4,3),]
            } else if (a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') {
              a1 <- a[c(1,5),]; a2 <- a[c(4,3),]
           } else if (a$period[1] == '202203' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') {
             a <- a[c(2,1,4,3,5), ]; a1 <- a[5,]; a2 <- a[1:4,]
           } else {stop("processingFives_s3e12: Iztrūkst apstrādes koda!\n")}
  } else if (all(a$sak_beidz[c(2,3,5)] == "1")) {
            if ((a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') ||
                (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') ||
                (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') ||
                (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') ||
                (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') ||
                (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________')) {
                  a1 <- a[5,]; a2 <- a[c(2,1,3,4),]
          } else {stop("processingFives_s3e12: Iztrūkst apstrādes koda!\n")}
  } else if (all(a$sak_beidz[c(1,4,5)] == "1")) {
             if ((a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________' && a$period[1] == '______') ||
                 (a$PS_code[1] %in% c("PK916C53B21", "PKB6A1D9CB3") && a$NM_code[1] ==  '______________' && a$period[1] == '______')) {
               a <- a %>% slice(1,2,4,3,5); a1 <- a[5,]; a2 <- a[1:4,]
             } else {stop("processingFives_s3e12: Iztrūkst apstrādes koda!\n")}
             if (kods %in% c("40", "50", "53") && o == "5") {ZERO_minus(a %>% slice(1))}
  } else if (all(a$sak_beidz[3:5] == "1")) {
             if (kods == "40") {
               if (a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________' && a$period[1] == '______') {
                 a <- a %>% slice(1,2,4,3,5); a1 <- a[5,]; a2 <- a[1:4,]
               } else {stop("processingFives_s3e12: Iztrūkst apstrādes koda!\n")}
             } else if (kods == "40") {
               if (a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________' && a$period[1] == '______') {
                 a <- a %>% slice(1,2,4,3,5); a1 <- a[5,]; a2 <- a[1:4,]
               } else {stop("processingFives_s3e12: Iztrūkst apstrādes koda!\n")}
             } else {stop("processingFives_s3e12: Iztrūkst apstrādes koda!\n")}
  } else {stop("processingFives_s3e12: Iztrūkst apstrādes koda!\n")}
  
  rm(a, o, kods)
  return(list(x5s3_uzVieniniekiem = a1, 
              x5s3_uzDivniekiem = a2,
              x5s3_uzCetriniekiem = a4)) 
}

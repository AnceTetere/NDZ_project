processingFives_s3e11 <- function(a, o, kods) {
  a1 <- data.frame(); a2 <- data.frame(); a4 <- data.frame()
  #a <- x5s3 for testing
  
  if (all(a$sak_beidz == c("2", "1", "2", "1", "1"))) {
    a1 <- a[c(1,5),]; a2 <- a[2:3,]
  } else if (all(a$sak_beidz == c("1", "2", "2", "1", "1"))) {
    a <- a[c(1,2,4,3,5), ]; a1 <- a[5,]; a2 <- a[1:4,]
    if (kods %in% c("40", "50", "53") && o == "5") {ZERO_minus(a %>% slice(1))}
  } else if (all(a$sak_beidz == c("2", "1", "1", "2", "1"))) {
    if ((a$period[1] == "_____" && a$PS_code[1] == "_____" && a$NM_code[1] == "_____") ||
        (a$period[1] == "_____" && a$PS_code[1] == "_____" && a$NM_code[1] == "_____")) {
      a1 <- a[c(1,5),]; a2 <- a[3:4,]
    } else if (a$period[1] == "_____" && a$PS_code[1] == "_____" && a$NM_code[1] == "_____") {
      a1 <- a[c(1,5), ]; a2 <- a[c(1,3),]
    } else {stop("processingFives_s3e11: Iztrūkst apstrādes koda!\n")}
  } else {stop("processingFives_s3e11: Iztrūkst apstrādes koda!\n")}
  
  rm(a, o, kods)
  return(list(x5s3_uzVieniniekiem = a1, 
              x5s3_uzDivniekiem = a2,
              x5s3_uzCetriniekiem = a4)) 
}

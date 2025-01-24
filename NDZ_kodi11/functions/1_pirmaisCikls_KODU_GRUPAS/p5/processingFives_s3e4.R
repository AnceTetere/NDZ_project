#GIT 20250124
processingFives_s3e4 <- function(a, kods) {
  a1 <- data.frame(); a2 <- data.frame(); a4 <- data.frame()
  #a <- x5s3 for testing
  if (all(a$sak_beidz[c(1,3,5)] == "1")) {
            if((a$PS_code[1] == "_________" && a$NM_code[1] == "_________") ||
               (a$PS_code[1] == "_________" && a$NM_code[1] == "_________")) {
              a1 <- rbind(a1, a[5, ])
              a4 <- rbind(a4, a[-5, ])
            } else {stop("processingFives_s3 iztrūkst apstrādes koda")}
    if (kods %in% c("40", "50", "53")) {ZERO_minus(a %>% slice(1))}
  } else if (all(a$sak_beidz[3:5] == "1")) {
           if(a$PS_code[1] == "_________" && a$NM_code[1] == "_________") {
             a1 <- rbind(a1, a[1, ])
           } else {stop("processingFives_s3e4 iztrūkst apstrādes koda")}
  } else if (all(a$sak_beidz[c(1,2,5)] == "1")) {
    if (a$period[1] == "_________" && a$PS_code[1] %in% c("_________", "_________") && a$NM_code[1] == "_________") {
      a1 <- rbind(a1, a[5, ])
      a4 <- rbind(a4, a[c(1,3,2,4),])
    } else {stop("processingFives_s3e4 iztrūkst apstrādes koda")}
    if (kods %in% c("40", "50", "53")) {ZERO_minus(a %>% slice(1))}
  } else {stop("processingFives_s3e4 iztrūkst apstrādes koda")} 
  
  rm(a)
  return(list(x5s3_uzVieniniekiem = a1, 
              x5s3_uzDivniekiem = a2,
              x5s3_uzCetriniekiem = a4)) 
}

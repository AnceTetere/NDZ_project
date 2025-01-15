processingFives_s3e4 <- function(a) {
  a1 <- data.frame(); a2 <- data.frame(); a4 <- data.frame()
  
  if (all(a$sak_beidz[c(1,3,5)] == "1")) {
    if((a$PS_code[1] == "____________" && a$NM_code[1] == "____________") ||
       (a$PS_code[1] == "____________" && a$NM_code[1] == "____________")) {
      a1 <- rbind(a1, a[5, ])
      a4 <- rbind(a4, a[-5, ])
    } else {stop("processingFives_s3 iztrūkst apstrādes koda")}
  } else if (all(a$sak_beidz == c("2", "2", "1", "1", "1"))) {
    if(a$PS_code[1] == "____________" && a$NM_code[1] == "____________") {
      a1 <- rbind(a1, a[1, ])
    } else {stop("processingFives_s3e4 iztrūkst apstrādes koda")}
  } else {stop("processingFives_s3e4 iztrūkst apstrādes koda")} 
  
  rm(a)
  return(list(x5s3_uzVieniniekiem = a1, 
              x5s3_uzDivniekiem = a2,
              x5s3_uzCetriniekiem = a4)) 
}

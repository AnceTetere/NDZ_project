processingFives_s2e2 <- function(a, kods) {  
  #a <- a for testing
  a <- a %>% arrange(PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
  a1 <- data.frame(); a2 <- data.frame(); a4 <- data.frame()
  
  if (all(a$sak_beidz[1:5] == c("2", "1", "2", "1", "2")) && a$zinkod[3] == "26") {
    a1 <- rbind(a1, a[1, ])
    a4 <- rbind(a4, a[-1, ])
  } else {stop("processingFives_s2: Trūkst apstrādes koda.")}
  
  rm(a, kods)
  return(list(x5s2_uzVieniniekiem = a1,
              x5s2_uzDivniekiem = a2,
              x5s2_uzCetriniekiem = a4))
}

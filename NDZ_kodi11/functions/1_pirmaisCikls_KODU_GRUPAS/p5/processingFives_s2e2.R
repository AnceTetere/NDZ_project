processingFives_s2e2 <- function(a, kods) {  
  #a <- x5s2 for testing
  a <- a %>% arrange(PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
  a1 <- data.frame(); a2 <- data.frame(); a4 <- data.frame()
  
  if (all(x5s2$sak_beidz[1:5] == c("2", "1", "2", "1", "2")) && x5s2$zinkod[3] == "26") {
    x5s2_uzVieniniekiem <- rbind(x5s2_uzVieniniekiem, x5s2[1, ])
    x5s2_uzCetriniekiem <- rbind(x5s2_uzCetriniekiem, x5s2[-1, ])
  } else {stop("processingFives_s2: Trūkst apstrādes koda.")}
  
  rm(a, kods)
  return(list(x5s2_uzVieniniekiem = a1,
              x5s2_uzDivniekiem = a2,
              x5s2_uzCetriniekiem = a4))
}

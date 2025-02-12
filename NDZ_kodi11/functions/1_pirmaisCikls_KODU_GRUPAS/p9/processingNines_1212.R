processingNines_1212 <- function(a, o, kods) {  
  #a <- a for testing
  a <- a %>% arrange(PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
  a1 <- data.frame(); a2 <- data.frame(); a6 <- data.frame(); a7 <- data.frame(); a8 <- data.frame()
  
  if (all(diff(a$NDZ_sanemsanas_datums[1:4]) != 0)) {
         a2 <- rbind(a2, a[1:2, ])
         a7 <- rbind(a7, a[3:9, ])
           if (kods %in% c("40", "50", "53") && o == "9") {ZERO_minus(a %>% slice(1))}
  } else {stop("processingNines_1212 iztrūkst apstrādes kods. \n")}
  
  rm(a, kods)
  return(list(x9_uzVieniniekiem = a1,
              x9_uzDivi = a2,
              x9_uzSesi = a6,
              x9_uzSeptini = a7,
              x9_uzAstoniekiem = a8))
}

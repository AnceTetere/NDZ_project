processingNines_1211 <- function(a, o, kods) {  
  #a <- x9 for testing
  a <- a %>% arrange(PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
  a1 <- data.frame(); a2 <- data.frame(); a6 <- data.frame(); a7 <- data.frame(); a8 <- data.frame()
  
  if (diff(a$NDZ_sanemsanas_datums[2:3]) == 0 && all(sapply(c(1,3:5), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
      #JO PIRMOREIZ
       if (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') {
           a2 <- a[1, ]; a2 <- a[c(3,2), ]; a6 <- a[c(4:9),]
           if (kods %in% c("40", "50", "53") && o == "9") {ZERO_minus(a %>% slice(2))}
       } else {stop("processingNines_1211 iztrūkst apstrādes kods. \n")}
  } else {stop("processingNines_1211 iztrūkst apstrādes kods. \n")}
  
  rm(a, kods)
  return(list(a_uzVieniniekiem = a1,
              a_uzDivi = a2,
              a_uzSesi = a6,
              a_uzSeptini = a7,
              a_uzAstoniekiem = a8))
} 

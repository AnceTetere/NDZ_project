processingNines_2211 <- function(a, o, kods) {  
  #a <- x9 for testing 
  a <- a %>% arrange(PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
  a1 <- data.frame(); a2 <- data.frame(); a6 <- data.frame(); a7 <- data.frame(); a8 <- data.frame()
  
  if (all(a$sak_beidz[8:9] == c("2", "1"))) {
    if (diff(a$NDZ_sanemsanas_datums[2:3]) == 0 && all(sapply(c(1,3), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
      if ((a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') ||
          (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________')) {
            a1 <- a[1, ]; a8 <- a[c(3,2,4,5,6,7,9,8), ]
      } else {stop("processingNines() iztrūkst apstrādes kods. \n")}
    } else {stop("processingNines() iztrūkst apstrādes kods. \n")}
  } else {stop("processingNines_1221 iztrūkst apstrādes kods. \n")}
  
  rm(a, kods)
  return(list(x9_uzVieniniekiem = a1,
              x9_uzDivi = a2,
              x9_uzSesi = a6,
              x9_uzSeptini = a7,
              x9_uzAstoniekiem = a8))
}

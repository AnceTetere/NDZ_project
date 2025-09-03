processingNines_2122 <- function(a, o, kods) {  
  #a <- x9 for testing !!!PIEVĒRS UZMANĪBU KOMPANIJAI
  a <- a %>% arrange(PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
  a1 <- data.frame(); a2 <- data.frame(); a5 <- data.frame(); a6 <- data.frame(); a7 <- data.frame(); a8 <- data.frame()
  
  if (diff(a$NDZ_sanemsanas_datums[4:5]) == 0 && all(diff(a$NDZ_sanemsanas_datums[1:4]) != 0)) {
           
           if ((a$period[1] ==  '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') ||
               (a$period[1] ==  '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') ||
               (a$period[1] ==  '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') ||
               (a$period[1] ==  '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________')) {
               a1 <- a[1, ]; a8 <- a[2:9, ]
           } else {stop("processingNines_2122() iztrūkst apstrādes kods. \n")}
  } else {stop("processingNines_2122() iztrūkst apstrādes kods. \n")}
  
  rm(a, kods)
  
  return(list(x9_uzVieniniekiem = a1,
              x9_uzDivi = a2,
              x9_uzPieci = a5,
              x9_uzSesi = a6,
              x9_uzSeptini = a7,
              x9_uzAstoniekiem = a8))
}

processingNines_2212 <- function(a, o, kods) {  
  a <- a %>% arrange(PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
  a1 <- data.frame(); a2 <- data.frame(); a6 <- data.frame(); a7 <- data.frame(); a8 <- data.frame()
  
if (diff(a$NDZ_sanemsanas_datums[2:3]) == 0 && all(sapply(c(1,3), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
      #JO PIRMOREIZ
      if ((a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') ||
          (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') ||
          (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') ||  
          (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________')) {
            a1 <- a[1, ]; a8 <- a[c(3,2,4:9), ]
      } else {stop("processingNines() iztrūkst apstrādes kods. \n")}
} else if (all(sapply(c(1,3,5,7), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) != 0)) &&
    all(sapply(c(2,4,6,8), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) == 0))) {
    if (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') {
        a1 <- a[1, ]; a8 <- a[c(3,2,4:9),]
   } else {stop("processingNines() iztrūkst apstrādes kods. \n")}
} else if (all(sapply(c(1,3,4,6,7), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) != 0)) &&
           all(sapply(c(2,5,8), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) == 0))) {
           if (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') {
               a1 <- a[1, ]; a8 <- a[c(3,2,4:9),]
          } else {stop("processingNines() iztrūkst apstrādes kods. \n")}
} else if (all(sapply(c(1,3), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) != 0)) &&
           all(sapply(c(2,4), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) == 0))) {
           if (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') {
                a1 <- a[1, ]; a2 <- a[c(3,2),]; a6 <- a[4:9,]
           } else {stop("processingNines() iztrūkst apstrādes kods. \n")}
} else {stop("processingNines() iztrūkst apstrādes kods. \n")}

  
  rm(a, kods)
  return(list(x9_uzVieniniekiem = a1,
              x9_uzDivi = a2,
              x9_uzSesi = a6,
              x9_uzSeptini = a7,
              x9_uzAstoniekiem = a8))
}

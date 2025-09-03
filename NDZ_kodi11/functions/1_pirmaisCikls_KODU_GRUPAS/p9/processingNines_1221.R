 processingNines_1221 <- function(a, o, kods) {  
  #a <- x9 for testing !!!PIEVĒRS UZMANĪBU KOMPANIJAI
   a <- a %>% arrange(PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
   a1 <- data.frame(); a2 <- data.frame(); a6 <- data.frame(); a7 <- data.frame(); a8 <- data.frame()
  
  if (diff(a$NDZ_sanemsanas_datums[3:4]) == 0 && 
      all(sapply(c(1,2,4,5), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
           #JO PIRMOREIZ
           if ((a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') ||
               (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') ||
               (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') ||
               (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') ||
               (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') ||
               (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') ||
               (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') ||
               (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________')) {
                 a2 <- a[1:2, ]; a7 <- a[c(4,3,5:9), ]
                 if (kods %in% c("40", "50", "53") && o == "9") {ZERO_minus(a %>% slice(1))}
           } else {stop("processingNines1221() iztrūkst apstrādes kods. \n")}
  } else if (all(sapply(c(3,5), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
             all(sapply(c(1,2,4), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
             #JO PIRMOREIZ
             if ((a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') ||
                 (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') ||
                 (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') ||
                 (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________')) {
                  a2 <- a[1:2, ]; a7 <- a[c(4,3,5:9), ]
                  if (kods %in% c("40", "50", "53") && o == "9") {ZERO_minus(a %>% slice(1))}
             } else {stop("processingNines1221() iztrūkst apstrādes kods. \n")}
  } else if (all(sapply(c(1,3), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
             all(sapply(c(2,4,5), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
             #JO PIRMOREIZ
             if (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') {
                 a2 <- a[1:2, ]; a7 <- a[c(4,3,5:9), ]
                 if (kods %in% c("40", "50", "53") && o == "9") {ZERO_minus(a %>% slice(1))}
             } else {stop("processingNines1221() iztrūkst apstrādes kods. \n")}
  } else {stop("processingNines_1221() iztrūkst apstrādes kods. \n")}
  
  rm(a, kods)
  
  return(list(x9_uzVieniniekiem = a1,
              x9_uzDivi = a2,
              x9_uzSesi = a6,
              x9_uzSeptini = a7,
              x9_uzAstoniekiem = a8))
}

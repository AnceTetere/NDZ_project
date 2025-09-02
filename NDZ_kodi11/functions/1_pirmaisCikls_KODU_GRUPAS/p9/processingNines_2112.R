processingNines_2112 <- function(a, o, kods) {  
  #a <- x9 for testing !!!PIEVĒRS UZMANĪBU KOMPANIJAI
  a <- a %>% arrange(PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
  a1 <- data.frame(); a2 <- data.frame(); a6 <- data.frame(); a7 <- data.frame(); a8 <- data.frame()
  
  if (diff(a$NDZ_sanemsanas_datums[1:2]) == 0 && all(diff(a$NDZ_sanemsanas_datums[2:5]) != 0)) {
              #JO PIRMOREIZ
               if ((a$period[1] == '______' && a$PS_code[1] ==  '______________' && aNM_code ==  '______________') ||
                   (a$period[1] == '______' && a$PS_code[1] ==  '______________' && aNM_code ==  '______________') ||
                   (a$period[1] == '______' && aNM_code ==  '______________') ||
                   (a$period[1] == '______' && aNM_code ==  '______________') ||
                   (a$period[1] == '______' && aNM_code ==  '______________') ||
                   (a$period[1] == '______' && a$PS_code[1] ==  '______________' && aNM_code ==  '______________') ||
                   (a$period[1] == '______' && a$PS_code[1] ==  '______________' && aNM_code ==  '______________') ||
                   (a$period[1] == '______' && a$PS_code[1] ==  '______________' && aNM_code ==  '______________')) {
                    a2 <- a[c(2,1), ]; a7 <- a[3:9, ]
                    if (kods %in% c("40", "50", "53") && o == "9") {ZERO_minus(a %>% slice(2))}
               } else {stop("processingNines2112() iztrūkst apstrādes kods. \n")}
  } else if (all(sapply(c(1,3), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) && 
             all(sapply(c(2,4), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
             #JO PIRMOREIZ
             if ((a$period[1] == '______' && a$PS_code[1] ==  '______________' && aNM_code ==  '______________') ||
                 (a$period[1] == '______' && a$PS_code[1] ==  '______________' && aNM_code ==  '______________') ||
                 (a$period[1] == '______' && a$PS_code[1] ==  '______________' && aNM_code ==  '______________') ||
                 (a$period[1] == '______' && a$PS_code[1] ==  '______________' && aNM_code ==  '______________')) {
                 a2 <- a[c(2,1), ]; a7 <- a[3:9, ]
                 if (kods %in% c("40", "50", "53") && o == "9") {ZERO_minus(a %>% slice(2))}
             } else {stop("processingNines2112() iztrūkst apstrādes kods. \n")} 
  } else if (all(diff(a$NDZ_sanemsanas_datums[1:5]) != 0)) {
             #JO PIRMOREIZ
             if ((a$period[1] == '______' && a$PS_code[1] ==  '______________' && aNM_code ==  '______________') ||
                 (a$period[1] == '______' && a$PS_code[1] ==  '______________' && aNM_code ==  '______________') ||
                 (a$period[1] == '______' && a$PS_code[1] ==  '______________' && aNM_code ==  '______________') ||
                 (a$period[1] == '______' && a$PS_code[1] ==  '______________' && aNM_code ==  '______________') ||
                 (a$period[1] == '______' && a$PS_code[1] ==  '______________' && aNM_code ==  '______________')) {
                 a1 <- a[1, ]; a7 <- a[3:9, ]
            } else {stop("processingNines2112() iztrūkst apstrādes kods. \n")} 
  } else {stop("processingNines_2112() iztrūkst apstrādes kods. \n")}
  
  rm(a, kods)
  return(list(x9_uzVieniniekiem = a1,
              x9_uzDivi = a2,
              x9_uzSesi = a6,
              x9_uzSeptini = a7,
              x9_uzAstoniekiem = a8))
}

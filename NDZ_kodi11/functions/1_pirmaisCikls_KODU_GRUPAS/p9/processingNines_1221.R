processingNines_1221 <- function(a, o, kods) {  
  a <- a %>% arrange(PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
  a1 <- data.frame(); a2 <- data.frame(); a6 <- data.frame(); a7 <- data.frame(); a8 <- data.frame()
  
  if (all(a$sak_beidz == c("1", "2", "2", "1", "1", "2", "1", "2", "1")) && 
           all(sapply(seq(1, 4, by = 2), function(i) all(diff(a$NDZ_sanemsanas_datums[i:i+1]) == 0))) &&
           all(diff(a$NDZ_sanemsanas_datums[4:9]) != 0)) {
            if (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') {
              a1 <- a[9, ]; a8 <- a[1:8, ]
              if (kods %in% c("40", "50", "53") && o == "9") {ZERO_minus(a %>% slice(1))}
            } else {stop("processingNines_1221 iztrūkst apstrādes kods. \n")}
  } else if (diff(a$NDZ_sanemsanas_datums[1:2]) != 0 && diff(a$NDZ_sanemsanas_datums[8:9]) != 0) {
            if ((a$period[1] == '______' && a$NM_code[1] ==  '______________') ||
                (a$period[1] == '______' && a$NM_code[1] ==  '______________') ||
                (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') ||
                (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') ||
                (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') ||
                (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') ||
                (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') ||
                (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________')) {
              a1 <- a[9, ]; a8 <- a[1:8, ]
               if (kods %in% c("40", "50", "53") && o == "9") {ZERO_minus(a %>% slice(1))}
             } else {stop("processingNines_1221 iztrūkst apstrādes kods. \n")}
  } else if (all(sapply(c(2,4,8), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) && diff(a$NDZ_sanemsanas_datums[1:2]) != 0) {
               if (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') {
                 a1 <- rbind(a1, a[9, ])
                 a8 <- rbind(a8, a[1:8, ])
                 if (kods %in% c("40", "50", "53") && o == "9") {ZERO_minus(a %>% slice(1))}
               } else {stop("processingNines_1221 iztrūkst apstrādes kods. \n")}
  } else if (diff(a$NDZ_sanemsanas_datums[1:2]) == 0 && all(diff(a$NDZ_sanemsanas_datums[2:9]) != 0)) {
              if (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') {
                a1 <- a[9, ]
                a8 <- a[1:8, ]
                if (kods %in% c("40", "50", "53") && o == "9") {ZERO_minus(a %>% slice(1))}
              } else {stop("processingNines_1221 iztrūkst apstrādes kods. \n")}
  } else if (diff(a$NDZ_sanemsanas_datums[1:2]) == 0 && all(diff(a$NDZ_sanemsanas_datums[8:9]) != 0)) {
             if (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') {
               a1 <- a[9, ]
               a8 <- a[1:8, ]
               if (kods %in% c("40", "50", "53") && o == "9") {ZERO_minus(a %>% slice(1))}
             } else {stop("processingNines_1221 iztrūkst apstrādes kods. \n")}
  } else {stop("processingNines_1221 iztrūkst apstrādes kods. \n")}
  
  rm(a, kods)
  return(list(x9_uzVieniniekiem = a1,
              x9_uzDivi = a2,
              x9_uzSesi = a6,
              x9_uzSeptini = a7,
              x9_uzAstoniekiem = a8))
}

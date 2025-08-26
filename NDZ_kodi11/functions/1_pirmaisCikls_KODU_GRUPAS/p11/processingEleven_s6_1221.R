processingEleven_s6_1221 <- function(b, o, kods) {
  b1 <- data.frame(); b2 <- data.frame(); b9 <- data.frame(); b10 <- data.frame()
  #b <- a
  
  if (all(sapply(c(1,2,4,5,6,8,9,10), function(i) diff(b$NDZ_sanemsanas_datums[i:(i+1)]) != 0)) &&
      all(sapply(c(3,7), function(i) diff(b$NDZ_sanemsanas_datums[i:(i+1)]) == 0))) {
           #JO PIRMOREIZ
           if (b$period[1] == '______' && b$PS_code[1] ==  '______________' && b$NM_code[1] ==  '______________') {
             b2 <- b[1:2, ]; b9 <- b[3:11,]
           } else {stop("processingEleven_s6_1221 trūkst izstrādes koda. \n")}
  } else if (all(sapply(c(1,2,4,6,8,10), function(i) diff(b$NDZ_sanemsanas_datums[i:(i+1)]) != 0)) &&
             all(sapply(seq(3,9,by=2), function(i) diff(b$NDZ_sanemsanas_datums[i:(i+1)]) == 0))) {
            #JO PIRMOREIZ
            if (b$period[1] == '______' && b$PS_code[1] ==  '______________' && b$NM_code[1] ==  '______________') {
              b2 <- b[1:2, ]; b9 <- b[3:11,]
              if (kods %in% c("40", "50", "53") && o == "11") {ZERO_minus(b %>% slice(1))}
            } else {stop("processingEleven_s6_1221 trūkst izstrādes koda. \n")}
  } else if (all(sapply(c(1,2,4,5,6,8,10), function(i) diff(b$NDZ_sanemsanas_datums[i:(i+1)]) != 0)) &&
             all(sapply(c(3,7,9), function(i) diff(b$NDZ_sanemsanas_datums[i:(i+1)]) == 0))) {
            #JO PIRMOREIZ
            if (b$period[1] == '______' && b$PS_code[1] ==  '______________' && b$NM_code[1] ==  '______________') {
              b2 <- b[1:2, ]; b9 <- b[3:11,]
              if (kods %in% c("40", "50", "53") && o == "11") {ZERO_minus(b %>% slice(1))}
            } else {stop("processingEleven_s6_1221 trūkst izstrādes koda. \n")}
  } else if (all(sapply(c(2,4,5), function(i) diff(b$NDZ_sanemsanas_datums[i:(i+1)]) != 0)) &&
             all(sapply(c(1,3), function(i) diff(b$NDZ_sanemsanas_datums[i:(i+1)]) == 0))) {
             #JO PIRMOREIZ
             if (b$period[1] == '______' && b$PS_code[1] ==  '______________' && b$NM_code[1] ==  '______________') {
                 b2 <- b[1:2, ]; b9 <- b[3:11,]
                 if (kods %in% c("40", "50", "53") && o == "11") {ZERO_minus(b %>% slice(1))}
             } else {stop("processingEleven_s6_1221 trūkst izstrādes koda. \n")}
  } else {stop("processingEleven_s6_1221 trūkst izstrādes koda. \n")}
  
rm(b, o, kods)
return(list(a1 = b1, a2 = b2, a9 = b9, a10 = b10))
}

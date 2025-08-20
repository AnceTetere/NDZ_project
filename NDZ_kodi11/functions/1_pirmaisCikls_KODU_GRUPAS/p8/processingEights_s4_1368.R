processingEights_s4_1368 <- function(a, o, kods) {

  #a <- x8s4
  a1 <- data.frame(); a2 <- data.frame(); a4 <- data.frame(); a5 <- data.frame(); a6 <- data.frame(); a7 <- data.frame()
  
    if (all(diff(a$NDZ_sanemsanas_datums) != 0)) {
              
              if (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') {
                a1 <- a[8, ]; a2 <- a[c(1,2,3,4,6,7), ]
                if (kods %in% c("40", "50", "53") && o == "8") {ZERO_plus(a %>% slice(5))} 
              } else {stop("processingEights_s4_1368: Trūkst izstrādes koda.")}
    } else if (diff(a$NDZ_sanemsanas_datums[3:4]) == 0 &&
               all(sapply(c(1,2,4,5,6,7), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
                if (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') {
                 a1 <- a[8, ]; a2 <- a[c(1,2,3,5,6,7), ]
                 if (kods %in% c("40", "50", "53") && o == "8") {ZERO_plus(a %>% slice(5))} 
                } else {stop("processingEights_s4_1368: Trūkst izstrādes koda.")}
    } else if (all(sapply(c(3,5,7), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
               all(sapply(c(1,2,4,6), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
                 if ((a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') ||
                     (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') ||
                     (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________')) {
                      a2 <- a[c(1,2,3,4,6,5,8,7),]
                      if (kods %in% c("40", "50", "53") && o == "8") {ZERO_minus(a %>% slice(1))} 
                } else {stop("processingEights_s4_1368: Trūkst izstrādes koda.")}
    } else if (all(sapply(c(5,7), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
               all(sapply(c(1,2,3,4,6), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
              if ((a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') ||
                  (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') ||
                  (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') ||
                  (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') ||
                  (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________')) {
                    a2 <- a[c(1,2,3,4,6,5,8,7),]
                    if (kods %in% c("40", "50", "53") && o == "8") {ZERO_minus(a %>% slice(1)); ZERO_plus(a %>% slice(8))} 
            } else {stop("processingEights_s4_1368: Trūkst izstrādes koda.")}
    } else {stop("processingEights_s4_1368: Trūkst izstrādes koda.")}

  rm(a, o, kods)

  return(list(x8s4_uzVieniniekiem = a1, 
              x8s4_uzDivniekiem = a2, 
              x8s4_uzCetriniekiem = a4, 
              x8s4_uz5 = a5, 
              x8s4_uzSesi = a6, 
              x8s4_uzSeptini = a7))
}

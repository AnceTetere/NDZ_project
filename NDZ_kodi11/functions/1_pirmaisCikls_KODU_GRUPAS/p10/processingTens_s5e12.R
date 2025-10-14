processingTens_s5e12 <- function(a, o, kods) {
  a1 <- data.frame(); a2 <- data.frame(); a5 <- data.frame(); a6 <- data.frame(); a7 <- data.frame(); a8 <- data.frame()
  #a <- x10s5

 if (a$sak_beidz[3] != "2") {
         if (all(diff(a$NDZ_sanemsanas_datums[1:3]) != 0)) {
              a2 <- a[1:2, ]; a8 <- a[3:10, ]
              if (kods %in% c("40", "50", "53") && o == "10") {ZERO_minus(a %>% slice(1))}
         } else if (diff(a$NDZ_sanemsanas_datums[1:2]) == 0 && diff(a$NDZ_sanemsanas_datums[2:3]) != 0) {
            if (aNM_code ==  '______________' ||
               (a$period[1] == '______' && a$PS_code[1] ==  '______________' && aNM_code ==  '______________') ||
               (a$period[1] == '______' && a$PS_code[1] ==  '______________' && aNM_code ==  '______________') ||
               (a$period[1] == '______' && a$PS_code[1] ==  '______________' && aNM_code ==  '______________') ||
               (a$period[1] == '______' && a$PS_code[1] ==  '______________' && aNM_code ==  '______________') ||
               (a$period[1] == '______' && a$PS_code[1] ==  '______________' && aNM_code ==  '______________') ||
               (a$period[1] == '______' && a$PS_code[1] ==  '______________' && aNM_code ==  '______________') ||
               (a$period[1] == '______' && a$PS_code[1] ==  '______________' && aNM_code ==  '______________') ||
               (a$period[1] == '______' && a$PS_code[1] ==  '______________' && aNM_code ==  '______________') ||
               (a$period[1] == '______' && a$PS_code[1] ==  '______________' && aNM_code ==  '______________')) {
                 a2 <- a[1:2, ]; a8 <- a[3:10, ]
                 if (kods %in% c("40", "50", "53") && o == "10") {ZERO_minus(a %>% slice(1))}
           } else {stop("processingTens_s5: Desmitnieku tabulas pārdalei trūkst izstrādes koda\n")}
         } else {stop("processingTens_s5: Desmitnieku tabulas pārdalei trūkst izstrādes koda\n")}
  } else if (a$sak_beidz[3] == "2") {
           if (a$sak_beidz[4] == "1") {
             if (all(a$sak_beidz[5:6] == c("1", "2"))) {
               if (all(diff(a$NDZ_sanemsanas_datums[1:5]) != 0)) {
                           if (a$period[1] == '______' && a$PS_code[1] ==  '______________' && aNM_code ==  '______________') {
                               a2 <- a[c(1,3), ]; a6 <- a[5:10, ]
                               if (kods %in% c("40", "50", "53") && o == "10") {ZERO_minus(a %>% slice(1))}
                           } else {stop("processingTens_s5: Desmitnieku tabulas pārdalei trūkst izstrādes koda\n")}
                } else if (all(sapply(c(1,3,5), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) && 
                           all(sapply(c(2,4), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
                             if ((a$period[1] == '______' && a$PS_code[1] ==  '______________' && aNM_code ==  '______________') ||
                                 (a$period[1] == '______' && a$PS_code[1] ==  '______________' && aNM_code ==  '______________') ||
                                 (a$period[1] == '______' && a$PS_code[1] ==  '______________' && aNM_code ==  '______________') ||
                                 (a$period[1] == '______' && a$PS_code[1] ==  '______________' && aNM_code ==  '______________') ||
                                 (a$period[1] == '______' && a$PS_code[1] ==  '______________' && aNM_code ==  '______________') ||
                                 (a$period[1] == '______' && a$PS_code[1] ==  '______________' && aNM_code ==  '______________') ||
                                 (a$period[1] == '______' && a$PS_code[1] ==  '______________' && aNM_code ==  '______________')) {
                                  a4 <- a[c(1,2,4,3), ]; a6 <- a[5:10, ]
                                  if (kods %in% c("40", "50", "53") && o == "10") {ZERO_minus(a %>% slice(1))}
                             } else {stop("processingTens_s5: Desmitnieku tabulas pārdalei trūkst izstrādes koda\n")}
                 } else if (diff(a$NDZ_sanemsanas_datums[3:4]) == 0 && 
                            all(sapply(c(1,2,4,5), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
                             if ((a$period[1] == '______' && a$PS_code[1] ==  '______________' && aNM_code ==  '______________') ||
                                 (a$period[1] == '______' && a$PS_code[1] ==  '______________' && aNM_code ==  '______________') ||
                                 (a$period[1] == '______' && aNM_code ==  '______________') ||
                                 (a$period[1] == '______' && aNM_code ==  '______________') ||
                                 (a$period[1] == '______' && aNM_code ==  '______________') ||
                                 (a$period[1] == '______' && aPS_code ==  '______________' && aNM_code ==  '______________') ||
                                 (a$period[1] == '______' && aPS_code ==  '______________' && aNM_code ==  '______________') ||
                                 (a$period[1] == '______' && aNM_code ==  '______________')) {
                                     a4 <- a[c(1,2,4,3), ]; a6 <- a[5:10, ]
                                     if (kods %in% c("40", "50", "53") && o == "10") {ZERO_minus(a %>% slice(1))}
                             } else {stop("processingTens_s5: Desmitnieku tabulas pārdalei trūkst izstrādes koda\n")}
                } else if (all(sapply(c(3,5), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) ||
                           all(sapply(c(1,2,4,6), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
                             if ((a$period[1] == '______' && a$PS_code[1] ==  '______________' && aNM_code ==  '______________') ||
                                 (a$period[1] == '______' && a$PS_code[1] ==  '______________' && aNM_code ==  '______________') ||
                                 (a$period[1] == '______' && a$PS_code[1] ==  '______________' && aNM_code ==  '______________') ||
                                 (a$period[1] == '______' && a$PS_code[1] ==  '______________' && aNM_code ==  '______________') ||
                                 (a$period[1] == '______' && a$PS_code[1] ==  '______________' && aNM_code ==  '______________') ||
                                 (a$period[1] == '______' && a$PS_code[1] ==  '______________' && aNM_code ==  '______________')) {
                                   a4 <- a[c(1,2,4,3), ]; a6 <- a[5:10, ]
                                   if (kods %in% c("40", "50", "53") && o == "10") {ZERO_minus(a %>% slice(1))}
                             } else {stop("processingTens_s5: Desmitnieku tabulas pārdalei trūkst izstrādes koda\n")}
                } else if (all(sapply(c(1,3,7), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) ||
                           all(sapply(c(2,4,5,6,8), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
                           if ((a$period[1] == '______' && a$PS_code[1] ==  '______________' && aNM_code ==  '______________') ||
                               (a$period[1] == '______' && a$PS_code[1] ==  '______________' && aNM_code ==  '______________') ||
                               (a$period[1] == '______' && a$PS_code[1] ==  '______________' && aNM_code ==  '______________') ||
                               (a$period[1] == '______' && a$PS_code[1] ==  '______________' && aNM_code ==  '______________')) {
                                a4 <- a[c(1,2,4,3), ]; a6 <- a[5:10, ]
                               if (kods %in% c("40", "50", "53") && o == "10") {ZERO_minus(a %>% slice(1))}
                           } else {stop("processingTens_s5: Desmitnieku tabulas pārdalei trūkst izstrādes koda\n")}
#      } else if (all(sapply(c(1,2,4), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) != 0)) && diff(a$NDZ_sanemsanas_datums[3:4]) == 0) {
#               if ((a$period[1] == '______' && a$PS_code[1] ==  '______________' && aNM_code ==  '______________') ||
#                   (a$period[1] == '______' && aNM_code ==  '______________') ||
#                   (a$period[1] == '______' && aNM_code ==  '______________') ||
#                   (a$period[1] == '______' && aNM_code ==  '______________') ||
#                   (a$period[1] == '______' && aNM_code ==  '______________') ||
#                   (a$period[1] == '______' && aNM_code ==  '______________')) {
#                     a2 <- a[1:2, ]; a8 <- a[c(4,3,5:10), ]
#               } else {stop("processingTens_s5: Desmitnieku tabulas pārdalei trūkst izstrādes koda\n")}
#                if (kods %in% c("40", "50", "53") && o == "10") {ZERO_minus(a %>% slice(1))}
#   } else if (all(sapply(c(1,3), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) && 
#              all(sapply(c(2,4), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
#              if ((a$period[1] == '______' && a$PS_code[1] ==  '______________' && aNM_code ==  '______________') ||
#                  (a$period[1] == '______' && aNM_code ==  '______________') ||
#                  (a$period[1] == '______' && a$PS_code[1] ==  '______________' && aNM_code ==  '______________') ||
#                  (a$period[1] == '______' && a$PS_code[1] ==  '______________' && aNM_code ==  '______________') ||
#                  (a$period[1] == '______' && a$PS_code[1] ==  '______________' && aNM_code ==  '______________') ||
#                   
#                  (a$period[1] == '______' && a$PS_code[1] ==  '______________' && aNM_code ==  '______________') ||
#                  (a$period[1] == '______' && a$PS_code[1] ==  '______________' && aNM_code ==  '______________')) {
#                      a2 <- a[1:2, ]; a8 <- a[c(4,3,5:10), ]
#              } else {stop("processingTens_s5: Desmitnieku tabulas pārdalei trūkst izstrādes koda\n")}
#              if (kods %in% c("40", "50", "53") && o == "10") {ZERO_minus(a %>% slice(1))}
           } else {stop("processingTens_s5: Desmitnieku tabulas pārdalei trūkst izstrādes koda\n")}
        } else if (all(a$sak_beidz[5:6] == c("2", "1"))) {
                 if (all(sapply(c(1,3), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) && 
                     all(sapply(c(2,4), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
                    if ((a$period[1] == '______' && a$PS_code[1] ==  '______________' && aNM_code ==  '______________') ||
                        (a$period[1] == '______' && a$PS_code[1] ==  '______________' && aNM_code ==  '______________') ||
                        (a$period[1] == '______' && a$PS_code[1] ==  '______________' && aNM_code ==  '______________') ||
                        (a$period[1] == '______' && a$PS_code[1] ==  '______________' && aNM_code ==  '______________') ||
                        (a$period[1] == '______' && a$PS_code[1] ==  '______________' && aNM_code ==  '______________') ||
                        (a$period[1] == '______' && a$PS_code[1] ==  '______________' && aNM_code ==  '______________') ||
                        (a$period[1] == '______' && a$PS_code[1] ==  '______________' && aNM_code ==  '______________') ||
                        (a$period[1] == '______' && a$PS_code[1] ==  '______________' && aNM_code ==  '______________') ||
                        (a$period[1] == '______' && a$PS_code[1] ==  '______________' && aNM_code ==  '______________')) {
                          a2 <- a[1:2, ]; a8 <- a[c(4,3,5:10), ] #no 69.rindas
                          if (kods %in% c("40", "50", "53") && o == "10") {ZERO_minus(a %>% slice(1))} #no 71. rindas
                    } else {stop("processingTens_s5: Desmitnieku tabulas pārdalei trūkst izstrādes koda\n")}
          } else if (all(sapply(c(3,5), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) && 
                     all(sapply(c(1,2,4), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
                     if ((a$period[1] == '______' && a$PS_code[1] ==  '______________' && aNM_code ==  '______________') ||
                         (a$period[1] == '______' && a$PS_code[1] ==  '______________' && aNM_code ==  '______________') ||
                         (a$period[1] == '______' && a$PS_code[1] ==  '______________' && aNM_code ==  '______________') ||
                         (a$period[1] == '______' && a$PS_code[1] ==  '______________' && aNM_code ==  '______________') ||
                         (a$period[1] == '______' && a$PS_code[1] ==  '______________' && aNM_code ==  '______________') ||
                         (a$period[1] == '______' && a$PS_code[1] ==  '______________' && aNM_code ==  '______________') ||
                         (a$period[1] == '______' && a$PS_code[1] ==  '______________' && aNM_code ==  '______________')) {
                             a2 <- a[1:2, ]; a8 <- a[c(4,3,6,5,7:10), ]
                             if (kods %in% c("40", "50", "53") && o == "10") {ZERO_minus(a %>% slice(1))}
                     } else {stop("processingTens_s5: Desmitnieku tabulas pārdalei trūkst izstrādes koda\n")}
          } else {stop("processingTens_s5: Desmitnieku tabulas pārdalei trūkst izstrādes koda\n")}
       } else {stop("processingTens_s5: Desmitnieku tabulas pārdalei trūkst izstrādes koda\n")}
    } else {stop("processingTens_s5: Desmitnieku tabulas pārdalei trūkst izstrādes koda\n")}
  } else {stop("processingTens_s5: Desmitnieku tabulas pārdalei trūkst izstrādes koda\n")}

rm(a, o, kods)

return(list(x10s5_1 = a1, 
            x10s5_2 = a2, 
            x10s5_5 = a5, 
            x10s5_6 = a6, 
            x10s5_7 = a7,
            x10s5_8 = a8))
}
 

 processingTens_s5e12 <- function(a, o, kods) {
  a1 <- data.frame(); a2 <- data.frame(); a5 <- data.frame(); a7 <- data.frame(); a8 <- data.frame()

 if (a$sak_beidz[3] != "2") {
         if (all(diff(a$NDZ_sanemsanas_datums[1:3]) != 0)) {
              a2 <- a[1:2, ]; a8 <- a[3:10, ]
              if (kods %in% c("40", "50", "53") && o == "10") {ZERO_minus(a %>% slice(1))}
         } else if (diff(a$NDZ_sanemsanas_datums[1:2]) == 0 && diff(a$NDZ_sanemsanas_datums[2:3]) != 0) {
            if (a$NM_code[1] == "__________" ||
               (a$period[1] == "_____" && a$PS_code[1] == "__________" && a$NM_code[1] == "__________") ||
               (a$period[1] == "_____" && a$PS_code[1] == "__________" && a$NM_code[1] == "__________") ||
               (a$period[1] == "_____" && a$PS_code[1] == "__________" && a$NM_code[1] == "__________") ||
               (a$period[1] == "_____" && a$PS_code[1] == "__________" && a$NM_code[1] == "__________")) {
                 a2 <- a[1:2, ]; a8 <- a[3:10, ]
                 if (kods %in% c("40", "50", "53") && o == "10") {ZERO_minus(a %>% slice(1))}
           } else {stop("processingTens_s5: Desmitnieku tabulas pārdalei trūkst izstrādes koda\n")}
         } else {stop("processingTens_s5: Desmitnieku tabulas pārdalei trūkst izstrādes koda\n")}
  } else if (all(a$sak_beidz[3:4] == c("2", "1"))) {
        if (all(sapply(c(1,2,4), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) != 0)) && diff(a$NDZ_sanemsanas_datums[3:4]) == 0) {
          if ((a$period[1] == "_____" && a$PS_code[1] == "__________" && a$NM_code[1] == "__________") ||
              (a$period[1] == "_____" && a$PS_code[1] == "__________" && a$NM_code[1] == "__________") ||
              (a$period[1] == "_____" && a$PS_code[1] == "__________" && a$NM_code[1] == "__________") ||
              (a$period[1] == "_____" && a$PS_code[1] == "__________" && a$NM_code[1] == "__________") ||
              (a$period[1] == "_____" && a$PS_code[1] == "__________" && a$NM_code[1] == "__________") ||
              (a$period[1] == "_____" && a$PS_code[1] == "__________" && a$NM_code[1] == "__________") ||
              (a$period[1] == "_____" && a$PS_code[1] == "__________" && a$NM_code[1] == "__________") ||
              (a$period[1] == "_____" && a$PS_code[1] == "__________" && a$NM_code[1] == "__________") ||
              (a$period[1] == "_____" && a$PS_code[1] == "__________" && a$NM_code[1] == "__________") ||
              (a$period[1] == "_____" && a$PS_code[1] == "__________" && a$NM_code[1] == "__________")) {
                a2 <- a[1:2, ]; a8 <- a[c(4,3,5:10), ]
          } else {stop("processingTens_s5: Desmitnieku tabulas pārdalei trūkst izstrādes koda\n")}
      if (kods %in% c("40", "50", "53") && o == "10") {ZERO_minus(a %>% slice(1))}
    } else if (all(sapply(c(1,3), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) && 
               all(sapply(c(2,4), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
      if ((a$period[1] == "_____" && a$PS_code[1] == "__________" && a$NM_code[1] == "__________") ||
          (a$period[1] == "_____" && a$PS_code[1] == "__________" && a$NM_code[1] == "__________") ||
          (a$period[1] == "_____" && a$PS_code[1] == "__________" && a$NM_code[1] == "__________") ||
          (a$period[1] == "_____" && a$PS_code[1] == "__________" && a$NM_code[1] == "__________") ||
          (a$period[1] == "_____" && a$PS_code[1] == "__________" && a$NM_code[1] == "__________") ||
          (a$period[1] == "_____" && a$PS_code[1] == "__________" && a$NM_code[1] == "__________")) {
              a2 <- a[1:2, ]; a8 <- a[c(4,3,5:10), ]
      } else {stop("processingTens_s5: Desmitnieku tabulas pārdalei trūkst izstrādes koda\n")}
      if (kods %in% c("40", "50", "53") && o == "10") {ZERO_minus(a %>% slice(1))}
    } else {stop("processingTens_s5: Desmitnieku tabulas pārdalei trūkst izstrādes koda\n")}
  } else {stop("processingTens_s5: Desmitnieku tabulas pārdalei trūkst izstrādes koda\n")}

rm(a, o, kods)

return(list(x10s5_1 = a1, 
            x10s5_2 = a2, 
            x10s5_5 = a5, 
            x10s5_7 = a7,
            x10s5_8 = a8))
}

#} else if (all(x10s5$sak_beidz[c(2, 5, 7, 9, 10)] == "1") && 
#           all(sapply(seq(4, 9, by = 2), function(i) diff(x10s5$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
#           all(sapply(seq(1, 4, by = 2), function(i) diff(x10s5$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
#  x10s5_1 <- rbind(x10s5_1, x10s5[1, ])
#  x10s5_2 <- rbind(x10s5_2, x10s5[2:3, ])
#  x10s5_7 <- rbind(x10s5_7, x10s5[4:10, ])
  #} else if (all(x10s5$sak_beidz[c(1, 2, 5, 6, 8)] == "2") && 
  #           all(x10s5$sak_beidz[c(3, 4, 7, 9, 10)] == "1") && 
  #           all(sapply(seq(6, 9, by = 2), function(i) all(diff(x10s5$NDZ_sanemsanas_datums[i:(i+1)]) == 0))) &&
  #           all(sapply(seq(3, 6, by = 2), function(i) all(diff(x10s5$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) &&
  #           all(diff(x10$NDZ_sanemsanas_datums[2:3]) == 0) &&
  #           all(diff(x10$NDZ_sanemsanas_datums[9:10]) != 0)) {
  #  x10s5_1 <- rbind(x10s5_1, x10[1, ])
  #  x10s5_2 <- rbind(x10s5_2, x10[2:3, ])
  #  x10s5_7 <- rbind(x10s5_7, x10[4:10, ])
#} else if (all(x10s5$sak_beidz[c(2, 5, 6, 9, 10)] == "1") && 
#           all(sapply(seq(4, 9, by = 2), function(i) diff(x10s5$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
#           all(sapply(c(1,3,9), function(i) diff(x10s5$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
#  x10s5_1 <- rbind(x10s5_1, x10s5[1, ])
#  x10s5_2 <- rbind(x10s5_2, x10s5[2:3, ])
#  x10s5_7 <- rbind(x10s5_7, x10s5[4:10, ])#


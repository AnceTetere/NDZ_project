processingTens_s5 <- function(x10s5, o, kods) {
  
  x10s5_1 <- data.frame(); x10s5_2 <- data.frame(); x10s5_5 <- data.frame(); x10s5_7 <- data.frame(); x10s5_8 <- data.frame()
  #x10s5 <- x10
  
  if (all(x10s5$sak_beidz[1:2] == c("1","2"))) {
           if (x10s5$sak_beidz[3] != "2") {
              if (all(diff(x10s5$NDZ_sanemsanas_datums[1:3]) != 0)) {
                      x10s5_2 <- rbind(x10s5_2, x10s5[1:2, ])
                      x10s5_8 <- rbind(x10s5_8, x10s5[3:10, ])
                      if (kods %in% c("40", "50", "53") && o == "10") {ZERO_minus(x10s5 %>% slice(1))}
              } else if (diff(x10s5$NDZ_sanemsanas_datums[1:2]) == 0 && diff(x10s5$NDZ_sanemsanas_datums[2:3]) != 0) {
                     if ((x10s5$period[1] == "______" && x10s5$PS_code[1] == "_______" && x10s5$NM_code[1] == "______") ||
                         (x10s5$period[1] == "______" && x10s5$PS_code[1] == "______" && x10s5$NM_code[1] == "______") ||
                         (x10s5$period[1] == "______" && x10s5$PS_code[1] == "______" && x10s5$NM_code[1] == "______") ||
                         (x10s5$period[1] == "______" && x10s5$PS_code[1] == "______" && x10s5$NM_code[1] == "______")) {
                         x10s5_2 <- rbind(x10s5_2, x10s5[1:2, ])
                         x10s5_8 <- rbind(x10s5_8, x10s5[3:10, ])
                     } else {stop("processingTens_s5: Desmitnieku tabulas pārdalei trūkst izstrādes koda\n")}
                     if (kods %in% c("40", "50", "53") && o == "10") {ZERO_minus(x10s5 %>% slice(1))}
                   } else {stop("processingTens_s5: Desmitnieku tabulas pārdalei trūkst izstrādes koda\n")}
               } else if (x10s5$sak_beidz[3] == "2") {
                   if (all(sapply(c(1,2,4), function(i) diff(x10s5$NDZ_sanemsanas_datums[i:(i+1)]) != 0)) && diff(x10s5$NDZ_sanemsanas_datums[3:4]) == 0) {
                       if (x10s5$period[1] == "______" && x10s5$PS_code[1] == "______" && x10s5$NM_code[1] == "______") {
                       x10s5_2 <- rbind(x10s5_2, x10s5[1:2, ])
                       x10s5_8 <- rbind(x10s5_8, x10s5[c(4,3,5:10), ])
                     } else {stop("processingTens_s5: Desmitnieku tabulas pārdalei trūkst izstrādes koda\n")}
                     if (kods %in% c("40", "50", "53") && o == "10") {ZERO_minus(x10s5 %>% slice(1))}
                   } else {stop("processingTens_s5: Desmitnieku tabulas pārdalei trūkst izstrādes koda\n")}
              } else {stop("processingTens_s5: Desmitnieku tabulas pārdalei trūkst izstrādes koda\n")}
  } else if (all(x10s5$sak_beidz[1:2] == c("2","1"))) {
            if (x10s5$sak_beidz[3] != "2") {
              if (diff(x10s5$NDZ_sanemsanas_datums[1:2]) == 0 && diff(x10s5$NDZ_sanemsanas_datums[2:3]) != 0) {
                if ((x10s5$period[1] == "______" && x10s5$PS_code[1] == "______" && x10s5$NM_code[1] == "______") ||
                    (x10s5$period[1] == "______" && x10s5$PS_code[1] == "______" && x10s5$NM_code[1] == "______") ||
                    (x10s5$period[1] == "______" && x10s5$PS_code[1] == "______" && x10s5$NM_code[1] == "______") ||
                    (x10s5$period[1] == "______" && x10s5$PS_code[1] == "______" && x10s5$NM_code[1] == "______") ||
                    (x10s5$period[1] == "______" && x10s5$PS_code[1] == "______" && x10s5$NM_code[1] == "______")) {
                  x10s5_2 <- rbind(x10s5_2, x10s5[c(2,1), ])
                  x10s5_8 <- rbind(x10s5_8, x10s5[3:10, ])
                } else {stop("processingTens_s5: Desmitnieku tabulas pārdalei trūkst izstrādes koda\n")}
                if (kods %in% c("40", "50", "53") && o == "10") {ZERO_minus(x10s5 %>% slice(2))}
              } else if (all(diff(x10s5$NDZ_sanemsanas_datums[1:6]) != 0)) {
                if (x10s5$sak_beidz[4] == "2") {
                  if (x10s5$sak_beidz[5] == "2") {
                    if (x10s5$sak_beidz[6] == "1") {
                        if (x10s5$period[1] == "______" && x10s5$PS_code[1] == "______" && x10s5$NM_code[1] == "______") {
                          x10s5_1 <- rbind(x10s5_1, x10s5[1, ])
                          x10s5_2 <- rbind(x10s5_2, x10s5[c(3,5), ])
                          x10s5_5 <- rbind(x10s5_5, x10s5[6:10, ])
                        } else {stop("processingTens_s5: Desmitnieku tabulas pārdalei trūkst izstrādes koda\n")}
                      } else {stop("processingTens_s5: Desmitnieku tabulas pārdalei trūkst izstrādes koda\n")}
                    } else {stop("processingTens_s5: Desmitnieku tabulas pārdalei trūkst izstrādes koda\n")}
                  } else {stop("processingTens_s5: Desmitnieku tabulas pārdalei trūkst izstrādes koda\n")}
                } else {stop("processingTens_s5: Desmitnieku tabulas pārdalei trūkst izstrādes koda\n")}
            } else if (all(x10s5$sak_beidz[3:4] == c("2", "1"))) { 
                         if (diff(x10s5$NDZ_sanemsanas_datums[1:2]) != 0) {
                           x10s5_1 <- rbind(x10s5_1, x10s5[1, ])
                           x10s5_2 <- rbind(x10s5_2, x10s5[2:3, ])
                           x10s5_7 <- rbind(x10s5_7, x10s5[4:10, ])
                         } else if (diff(x10s5$NDZ_sanemsanas_datums[1:2]) == 0 && diff(x10s5$NDZ_sanemsanas_datums[2:3]) != 0) {
                           if (x10s5$period[1] == "______" && x10s5$PS_code[1] == "______" && x10s5$NM_code[1] == "______") {
                             x10s5_2 <- rbind(x10s5_2, x10s5[c(2,1), ])
                             x10s5_8 <- rbind(x10s5_8, x10s5[3:10, ])
                           } else {stop("processingTens_s5: Desmitnieku tabulas pārdalei trūkst izstrādes koda\n")}
                           if (kods %in% c("40", "50", "53") && o == "10") {ZERO_minus(x10s5 %>% slice(2))}
                         } else {stop("processingTens_s5: Desmitnieku tabulas pārdalei trūkst izstrādes koda\n")}
            } else {stop("processingTens_s5: Desmitnieku tabulas pārdalei trūkst izstrādes koda\n")} 
  } else if (all(x10s5$sak_beidz[c(2, 5, 7, 9, 10)] == "1") && 
             all(sapply(seq(4, 9, by = 2), function(i) diff(x10s5$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
             all(sapply(seq(1, 4, by = 2), function(i) diff(x10s5$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
               x10s5_1 <- rbind(x10s5_1, x10s5[1, ])
               x10s5_2 <- rbind(x10s5_2, x10s5[2:3, ])
               x10s5_7 <- rbind(x10s5_7, x10s5[4:10, ])
    #} else if (all(x10s5$sak_beidz[c(1, 2, 5, 6, 8)] == "2") && 
    #           all(x10s5$sak_beidz[c(3, 4, 7, 9, 10)] == "1") && 
    #           all(sapply(seq(6, 9, by = 2), function(i) all(diff(x10s5$NDZ_sanemsanas_datums[i:(i+1)]) == 0))) &&
    #           all(sapply(seq(3, 6, by = 2), function(i) all(diff(x10s5$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) &&
    #           all(diff(x10$NDZ_sanemsanas_datums[2:3]) == 0) &&
    #           all(diff(x10$NDZ_sanemsanas_datums[9:10]) != 0)) {
    #  x10s5_1 <- rbind(x10s5_1, x10[1, ])
    #  x10s5_2 <- rbind(x10s5_2, x10[2:3, ])
    #  x10s5_7 <- rbind(x10s5_7, x10[4:10, ])
  } else if (all(x10s5$sak_beidz[c(2, 5, 6, 9, 10)] == "1") && 
             all(sapply(seq(4, 9, by = 2), function(i) diff(x10s5$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
             all(sapply(c(1,3,9), function(i) diff(x10s5$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
                x10s5_1 <- rbind(x10s5_1, x10s5[1, ])
                x10s5_2 <- rbind(x10s5_2, x10s5[2:3, ])
                x10s5_7 <- rbind(x10s5_7, x10s5[4:10, ])
  } else {stop("processingTens_s5: Desmitnieku tabulas pārdalei trūkst izstrādes koda\n")} 
  
  rm(x10s5, o, kods)
    
  return(list(x10_uzVieniniekiem = x10s5_1, 
              x10_uzDivniekiem = x10s5_2, 
              x10_uzPieci = x10s5_5, 
              x10_uzSeptini = x10s5_7,
              x10_uzAstoniekiem = x10s5_8))
}

processingTens_s5e21 <- function(a, o, kods) {
  a1 <- data.frame(); a2 <- data.frame(); a5 <- data.frame(); a6 <- data.frame(); a7 <- data.frame(); a8 <- data.frame()
  #a <- x10s5

  if (a$sak_beidz[3] != "2") {
    if (diff(a$NDZ_sanemsanas_datums[1:2]) == 0 && diff(a$NDZ_sanemsanas_datums[2:3]) != 0) {
      if (a$NM_code[1] ==  '______________' ||
          (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') ||
          (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') ||
          (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') ||
          (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') ||
          (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') ||
          (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') ||
          (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') ||
          (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________')) {
           a2 <- a[c(2,1), ]; a8 <- a[3:10, ]
           if (kods %in% c("40", "50", "53") && o == "10") {ZERO_minus(a %>% slice(2)); ZERO_plus(a %>% slice(9))}
      } else {stop("processingTens_s5e21: Desmitnieku tabulas pārdalei trūkst izstrādes koda\n")}
    } else if (all(diff(a$NDZ_sanemsanas_datums[1:6]) != 0)) {
      if (a$sak_beidz[4] == "2") {
        if (a$sak_beidz[5] == "2") {
          if (a$sak_beidz[6] == "1") {
            if (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') {
              a1 <- a[1, ]; a2 <- a[c(3,5), ]; a5 <- a[6:10, ]
            } else {stop("processingTens_s5e21: Desmitnieku tabulas pārdalei trūkst izstrādes koda\n")}
          } else {stop("processingTens_s5e21: Desmitnieku tabulas pārdalei trūkst izstrādes koda\n")}
        } else if (a$sak_beidz[5] == "1") {
                if (a$sak_beidz[6] == "2") {
                    if (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') {
                      a1 <- a[1, ]; a2 <- a[c(3,4), ]; a6 <- a[5:10, ]
                    } else {stop("processingTens_s5e21: Desmitnieku tabulas pārdalei trūkst izstrādes koda\n")}
                 } else {stop("processingTens_s5e21: Desmitnieku tabulas pārdalei trūkst izstrādes koda\n")}
        } else {stop("processingTens_s5e21: Desmitnieku tabulas pārdalei trūkst izstrādes koda\n")}
      } else {stop("processingTens_s5e21: Desmitnieku tabulas pārdalei trūkst izstrādes koda\n")}
    } else {stop("processingTens_s5e21: Desmitnieku tabulas pārdalei trūkst izstrādes koda\n")}
  } else if (all(a$sak_beidz[3:4] == c("2", "1"))) { 
    if (diff(a$NDZ_sanemsanas_datums[1:2]) != 0) {
      a1 <- a[1, ]; a2 <- a[2:3, ]; a7 <- a[4:10, ]
    } else if (diff(a$NDZ_sanemsanas_datums[1:2]) == 0 && diff(a$NDZ_sanemsanas_datums[2:3]) != 0) {
      if ((a$period[1] == '______' && a$NM_code[1] ==  '______________') ||
          (a$period[1] == '______' && a$NM_code[1] ==  '______________') ||
          (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') ||
          (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') ||
          (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') ||
          (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________')) {
              a2 <- a[c(2,1), ]; a8 <- a[3:10, ]
      } else {stop("processingTens_s5e21: Desmitnieku tabulas pārdalei trūkst izstrādes koda\n")}
      if (kods %in% c("40", "50", "53") && o == "10") {ZERO_minus(a %>% slice(2))}
    } else {stop("processingTens_s5e21: Desmitnieku tabulas pārdalei trūkst izstrādes koda\n")}
  } else if (all(a$sak_beidz[3:4] == c("2", "1"))) { 
    if (diff(a$NDZ_sanemsanas_datums[1:2]) != 0) {
      a1 <- a[1, ]; a2 <- a[2:3, ]; a7 <- a[4:10, ]
    } else if (diff(a$NDZ_sanemsanas_datums[1:2]) == 0 && diff(a$NDZ_sanemsanas_datums[2:3]) != 0) {
      if ((a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') ||
          (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') ||
          (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') ||
          (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') ||
          (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') ||
          (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') ||
          (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________')) {
        a2 <- a[c(2,1), ]; a8 <- a[3:10, ]
      } else {stop("processingTens_s5e21: Desmitnieku tabulas pārdalei trūkst izstrādes koda\n")}
      if (kods %in% c("40", "50", "53") && o == "10") {ZERO_minus(a %>% slice(2))}
    } else {stop("processingTens_s5e21: Desmitnieku tabulas pārdalei trūkst izstrādes koda\n")}
  } else {stop("processingTens_s5e21: Desmitnieku tabulas pārdalei trūkst izstrādes koda\n")}
  
  rm(a, o, kods)
  
  return(list(x10s5_1 = a1, 
              x10s5_2 = a2, 
              x10s5_5 = a5, 
              x10s5_6 = a6, 
              x10s5_7 = a7,
              x10s5_8 = a8))
}

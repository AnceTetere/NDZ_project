processingTwelve_b6 <- function(a, o, kods) {
  
  a1 <- data.frame(); a2 <- data.frame(); a7 <- data.frame(); a8 <- data.frame(); a10 <- data.frame(); a11 <- data.frame()
  #a <- x12
  
  if (a$sak_beidz[1] == "2") {
    if (a$sak_beidz[2] == "1") {
      if (a$sak_beidz[3] == "2") {
          if (diff(a$NDZ_sanemsanas_datums[1:2]) != 0) {
            a1 <- a[1, ]; a11 <- a[2:12, ]
          } else if (diff(a$NDZ_sanemsanas_datums[1:2]) == 0) {
            a2 <- a[1:2, ]; a10 <- a[-(1:2), ]
          } else {stop("12-nieku tabulas pārdalei trūkst izstrādes koda.")}
        } else if (a$sak_beidz[3] == "1") {
                 if (diff(a$NDZ_sanemsanas_datums[1:2]) == 0) {
                    #JO PIRMOREIZ - nobloķēšu tikai ar to kompāniju, jo tur visiem ir līdzīga sistemātika.
                     if (a$NM_code[1] ==  '______________') {
                         a2 <- a[c(2,1), ]; a10 <- a[3:12, ]
                    } else {stop("12-nieku tabulas pārdalei trūkst izstrādes koda.")}
                 } else {stop("12-nieku tabulas pārdalei trūkst izstrādes koda.")}
        } else {stop("12-nieku tabulas pārdalei trūkst izstrādes koda.")}
    } else if (a$sak_beidz[2] == "2") {
             if (a$sak_beidz[3] == "1") {
               if (a$sak_beidz[4] == "1") {
                 if (a$sak_beidz[5] == "2") {
                   if (a$sak_beidz[6] == "2") {
                     if (a$sak_beidz[7] == "1") {
                       if (a$sak_beidz[8] == "1") {
                         if (a$sak_beidz[9] == "2") {
                           if (a$sak_beidz[10] == "2") {
                             if (a$sak_beidz[11] == "1") {
                               if (a$sak_beidz[12] == "1") {
                                 if (all(sapply(c(1,3,5,7,9,11), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) && 
                                     all(sapply(c(2,4,6,8,10), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
                                   if (kods == "40") {
                                      #JO PIRMOREIZ - BK atvaļinājuma kodi lietoti dubultā
                                       if ((a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') ||
                                           (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________')) {
                                           a1 <- a[c(1,11), ]; a <- a[c(3,5,7,9),]
                                       } else {stop("12-nieku tabulas pārdalei trūkst izstrādes koda.")}
                                   } else {stop("12-nieku tabulas pārdalei trūkst izstrādes koda.")}
                                 } else {stop("12-nieku tabulas pārdalei trūkst izstrādes koda.")}
                              } else {stop("12-nieku tabulas pārdalei trūkst izstrādes koda.")}
                           } else {stop("12-nieku tabulas pārdalei trūkst izstrādes koda.")}
                        } else {stop("12-nieku tabulas pārdalei trūkst izstrādes koda.")}
                     } else {stop("12-nieku tabulas pārdalei trūkst izstrādes koda.")}
                  } else {stop("12-nieku tabulas pārdalei trūkst izstrādes koda.")}
                } else {stop("12-nieku tabulas pārdalei trūkst izstrādes koda.")}
              } else {stop("12-nieku tabulas pārdalei trūkst izstrādes koda.")}
            } else {stop("12-nieku tabulas pārdalei trūkst izstrādes koda.")}
          } else {stop("12-nieku tabulas pārdalei trūkst izstrādes koda.")}
        } else {stop("12-nieku tabulas pārdalei trūkst izstrādes koda.")}
      } else {stop("12-nieku tabulas pārdalei trūkst izstrādes koda.")} 
  } else if (a$sak_beidz[1] == "1") {           
           if (a$sak_beidz[2] == "2") { 
             if (a$sak_beidz[3] == "1") {
               if (diff(a$NDZ_sanemsanas_datums[2:3]) != 0) {
                          a2 <- a[1:2, ]; a10 <- a[-(1:2), ]
                          if (kods %in% c("40", "50", "53") && o == "12") {ZERO_minus(a %>% slice(1))}
              } else if (diff(a$NDZ_sanemsanas_datums[1:2]) != 0) {
                          a2 <- a[1:2, ]; a10 <- a[-(1:2), ]
                          if (kods %in% c("40", "50", "53") && o == "12") {ZERO_minus(a %>% slice(1))}
              } else {stop("12-nieku tabulas pārdalei trūkst izstrādes koda.")}
            } else if (a$sak_beidz[3] == "2") {
                     if (a$sak_beidz[4] == "1") {
                       if (a$sak_beidz[5] == "2") {
                        if (all(diff(a$NDZ_sanemsanas_datums[1:4]) != 0)) {
                                   a2 <- a[c(1,3), ]; a9 <- a[4:12, ]
                        } else if (diff(a$NDZ_sanemsanas_datums[1:2]) == 0 && diff(a$NDZ_sanemsanas_datums[2:3]) != 0) {
                                   #BLOĶĒJU, JO PIRMOREIZ
                                   if ((a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') ||
                                       (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________')) {
                                        a2 <- a[1:2, ]; a10 <- a[3:12, ]
                                    } else {stop("12-nieku tabulas pārdalei trūkst izstrādes koda.")}
                        } else if (all(diff(a$NDZ_sanemsanas_datums[1:3]) != 0) && diff(a$NDZ_sanemsanas_datums[3:4]) == 0) {
                                  #BLOĶĒJU, JO PIRMOREIZ
                                  if ((a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') ||
                                      (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') ||
                                      (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________')) {
                                       a2 <- a[1:2, ]; a10 <- a[3:12, ]
                                       if (kods %in% c("40", "50", "53") && o == "12") {ZERO_minus(a %>% slice(1))}
                                  } else {stop("12-nieku tabulas pārdalei trūkst izstrādes koda.")}
                        } else {stop("12-nieku tabulas pārdalei trūkst izstrādes koda.")}
                     } else if (a$sak_beidz[5] == "1") {
                              if (a$sak_beidz[6] == "2") {
                                if (all(sapply(c(1,3,5), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) && 
                                    all(sapply(c(2,4), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
                                        #BLOĶĒJU, JO PIRMOREIZ
                                         if ((a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') ||
                                             (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________')) {
                                                a2 <- a[1:2, ]; a10 <- a[c(4,3, 5:12), ]
                                         } else {stop("12-nieku tabulas pārdalei trūkst izstrādes koda.")}           
                                } else if (all(sapply(c(3,5), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) && 
                                           all(sapply(c(1,2,4), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
                                               #BLOĶĒJU, JO PIRMOREIZ
                                                if (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') {
                                                       a2 <- a[1:2, ]; a10 <- a[c(4,3, 5:12), ]
                                               } else {stop("12-nieku tabulas pārdalei trūkst izstrādes koda.")}  
                                }  else if (diff(a$NDZ_sanemsanas_datums[3:4]) == 0 && 
                                            all(sapply(c(1,2,4:6), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
                                            #BLOĶĒJU, JO PIRMOREIZ
                                            if ((a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') ||
                                                (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________')) {
                                                a2 <- a[1:2, ]; a10 <- a[c(4,3, 5:12), ]
                                            } else {stop("12-nieku tabulas pārdalei trūkst izstrādes koda.")}  
                                } else if (all(diff(a$NDZ_sanemsanas_datums[1:6]) != 0)) {
                                           #JO PIRMOREIZ
                                           if (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') {
                                               a2 <- a[c(1,3), ]; a8 <- a[5:12, ]
                                           } else {stop("12-nieku tabulas pārdalei trūkst izstrādes koda.")}  
                                }else {stop("12-nieku tabulas pārdalei trūkst izstrādes koda.")}
                             } else {stop("12-nieku tabulas pārdalei trūkst izstrādes koda.")}
                      } else {stop("12-nieku tabulas pārdalei trūkst izstrādes koda.")}
                  } else {stop("12-nieku tabulas pārdalei trūkst izstrādes koda.")}
               } else {stop("12-nieku tabulas pārdalei trūkst izstrādes koda.")}
           } else {stop("12-nieku tabulas pārdalei trūkst izstrādes koda.")}
       } else {stop("12-nieku tabulas pārdalei trūkst izstrādes koda.")}

  rm(a, o, kods)
  
  return(list(x12_uzVieniniekiem = a1,
              x12_uzDivniekiem = a2,
              x12_uzSeptini = a7,
              x12_uzAstoni = a8,
              x12_uzDesmitniekiem = a10,
              x12_uzVienpadsmit = a11))
}


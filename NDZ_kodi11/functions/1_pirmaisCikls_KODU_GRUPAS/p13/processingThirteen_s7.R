processingThirteen_s7 <- function(a, o, kods) {
  
  a1 <- data.frame(); a2 <- data.frame(); a3 <- data.frame(); a4 <- data.frame()
  a8 <- data.frame(); a10 <- data.frame(); a11 <- data.frame()
  #a <- x13
  
  if (a$sak_beidz[1] == "1") {
    if (a$sak_beidz[2] == "2") {
      if (a$sak_beidz[3] == "1") {
        if (diff(a$NDZ_sanemsanas_datums[1:2]) != 0 && diff(a$NDZ_sanemsanas_datums[2:3]) != 0) {
          if (all(a$sak_beidz[11:13] == c("1","2","1"))) {
            if (diff(a$NDZ_sanemsanas_datums[12:13]) != 0) {
              a3 <- a[11:13, ]; a10 <- a[1:10, ]
              if (kods %in% c("40", "50", "53") && o == "13") {ZERO_minus(a %>% slice(1))}
            } else {stop("processingThirteen_s7 trūkst izstrādes koda.\n")}
          } else if (all(a$sak_beidz[10:13] == c("2", "2", "1", "1"))) {
            if (all(sapply(c(10, 12), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) != 0)) && diff(a$NDZ_sanemsanas_datums[11:12]) == 0) {
              #Te viss kārtībā, BLOĶĒJU, JO PIRMO REIZ
              if (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') {
                a10 <- a[1:10,]; a3 <- a[c(12,11,13),]
                if (kods %in% c("40", "50", "53") && o == "13") {ZERO_minus(a %>% slice(1))}
              } else {stop("processingThirteen_s7 trūkst izstrādes koda.\n")}
            } else {stop("processingThirteen_s7 trūkst izstrādes koda.\n")}
          } else if (a$sak_beidz[9] == a$sak_beidz[10] && a$sak_beidz[11] == a$sak_beidz[13]) {
            if (diff(a$NDZ_sanemsanas_datums[10:11]) != 0) {
              a4 <- a[10:13, ]; a8 <- a[1:8, ]
              if (kods %in% c("40", "50", "53") && o == "13") {ZERO_minus(a %>% slice(1))}
            } else {stop("processingThirteen_s7 trūkst izstrādes koda.\n")}
          } else {stop("processingThirteen_s7 trūkst izstrādes koda.\n")}
        } else {stop("processingThirteen_s7 trūkst izstrādes koda.\n")}
      } else if (a$sak_beidz[3] == "2") {
               if (all(sapply(c(1,3,5), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
                   all(sapply(c(2,4), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
                   #JO PIRMOREIZ
                   if (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') {
                     a2 <- a[1:2,]; a11 <- a[3:13,]
                     if (kods %in% c("40", "50", "53") && o == "13") {ZERO_minus(a %>% slice(1))}
                   } else {stop("processingThirteen_s7 trūkst izstrādes koda.\n")}
               } else if (all(sapply(c(3,5), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
                          all(sapply(c(1,2,4), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
                          #JO PIRMOREIZ
                          if ((a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') ||
                              (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________')) {
                               a2 <- a[1:2,]; a11 <- a[c(4,3,5:13),]
                               if (kods %in% c("40", "50", "53") && o == "13") {ZERO_minus(a %>% slice(1))}
                          } else {stop("processingThirteen_s7 trūkst izstrādes koda.\n")}
               } else {stop("processingThirteen_s7 trūkst izstrādes koda.\n")}
      } else if (all(a$sak_beidz[c(3, 5, 7, 9, 11, 13)] == "1") && all(a$sak_beidz[c(4, 6, 8, 10, 12)] == "2")) {
        if (all(diff(a$NDZ_sanemsanas_datums[1:12]) != 0) && diff(a$NDZ_sanemsanas_datums[12:13]) == 0) {
          a1 <- a[13, ]; a4 <- a[1:4, ]; a8 <- a[5:12, ]
          if (kods %in% c("40", "50", "53") && o == "13") {ZERO_minus(a %>% slice(1))}
        } else {stop("processingThirteen_s7 trūkst izstrādes koda.\n")}  
      } else if (all(a$sak_beidz[3:4] == c("2", "1"))) {
        if (all(diff(a$NDZ_sanemsanas_datums[1:3]) != 0) && diff(a$NDZ_sanemsanas_datums[3:4]) == 0) {
          a2 <- a[1:2, ]; a11 <- a[c(4,3,5:13), ]
          if (kods %in% c("40", "50", "53") && o == "13") {ZERO_minus(a %>% slice(1))}                     
        } else {stop("processingThirteen_s7 trūkst izstrādes koda.\n")}  
      } else {stop("processingThirteen_s7 trūkst izstrādes koda.\n")} 
    } else if (a$sak_beidz[2] == "1") {
         if (a$sak_beidz[3] == "2") {
          if (a$sak_beidz[4] == "1") {
           if (a$sak_beidz[5] == "2") {
             if (all(sapply(c(2,4), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
                 all(sapply(c(1,3,5), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
                 #JO PIRMOREIZ
                 if (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') {
                   a2 <- a[c(1,3),]; a11 <- a[c(2,4:13), ] 
                 } else {stop("processingThirteen_s7 trūkst izstrādes koda.\n")} 
             } else {stop("processingThirteen_s7 trūkst izstrādes koda.\n")} 
            } else {stop("processingThirteen_s7 trūkst izstrādes koda.\n")} 
          } else {stop("processingThirteen_s7 trūkst izstrādes koda.\n")} 
         } else {stop("processingThirteen_s7 trūkst izstrādes koda.\n")} 
    } else {stop("processingThirteen_s7 trūkst izstrādes koda.\n")} 
  } else if (all(a$sak_beidz[1:4] == c("2", "1", "2", "1"))) {
    if (all(sapply(c(1,3), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) && diff(a$NDZ_sanemsanas_datums[2:3]) != 0) {
      a2 <- a[c(2,1), ]; a11 <- a[c(4,3,5:13), ]
      if (kods %in% c("40", "50", "53") && o == "13") {ZERO_minus(a %>% slice(1))}
    } else {stop("processingThirteen_s7 trūkst izstrādes koda.\n")}
  } else {stop("processingThirteen_s7 trūkst izstrādes koda.\n")}
  
  rm(a, o, kods)
  return(list(x13_uzVieniniekiem = a1, 
              x13_uzDivi = a2,
              x13_uzTrijniekiem = a3,
              x13_uzCetriniekiem = a4,
              x13_uzAstoniekiem = a8, 
              x13_uzDesmitniekiem = a10,
              x13_uzVienpadsmit = a11))
}

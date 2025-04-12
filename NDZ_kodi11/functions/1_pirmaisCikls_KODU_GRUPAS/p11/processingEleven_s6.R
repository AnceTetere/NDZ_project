processingEleven_s6 <- function(a, o, kods) {
  a1 <- data.frame(); a2 <- data.frame(); a9 <- data.frame(); a10 <- data.frame()
  
    if (all(a$sak_beidz[c(1,3,5,8,10,11)] == "1")) {
              if (all(sapply(c(1,2,3,4,6,8,10), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) != 0)) &&
                  all(sapply(c(5,7,9), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) == 0))) {
                a <- a[c(1,2,3,4,5,6,8,7,10,9,11),]; a1 <- a[11, ]; a10 <- a[-11,]
                if (kods %in% c("40", "50", "53") && o == "11") {ZERO_minus(a %>% slice(1))}
              } else if (all(sapply(c(1,3,7,9), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
                         all(sapply(c(2,4,5,6,8,10), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
                a <- a[c(1,2,3,4,5,6,8,7,10,9,11),]; a1 <- a[11, ]; a10 <- a[-11,]
                if (kods %in% c("40", "50", "53") && o == "11") {ZERO_minus(a %>% slice(1))}
              } else {stop("processingEleven: Vienpadsmitnieku tabulas pārdalei trūkst izstrādes koda. Rindas: ", r, " līdz ", r + 10, "\n")}
    } else if (all(a$sak_beidz[c(1,3,5,6,8,10)] == "1") && 
               all(diff(a$NDZ_sanemsanas_datums[c(1:3, 5:11)]) != 0) &&
               diff(a$NDZ_sanemsanas_datums[4:5]) == 0) {
                a10 <- a[-5,]
                if (kods %in% c("40", "50", "53") && o == "11") {ZERO_minus(a %>% slice(1)); ZERO_plus(a %>% slice(11))}
    } else if (all(a$sak_beidz[c(1,3,5,8,9,11)] == "1")) { 
              if (all(diff(a$NDZ_sanemsanas_datums[c(1:2, 4:7, 9:11)]) != 0) &&
                  all(sapply(c(3,7), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) == 0))) {
                        a1 <- a[11, ]; a10 <- a[-11,] 
              } else if (all(sapply(c(1:6,8:10), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) != 0)) &&
                         diff(a$NDZ_sanemsanas_datums[7:8]) == 0) {
                        a1 <- a[11, ]; a10 <- a[c(1,2,3,4,5,6,8,7,9,10),]
              } else {stop("processingEleven trūkst apstrādes koda. \n")}
              if (kods %in% c("40", "50", "53") && o == "11") {ZERO_minus(a %>% slice(1))}
    } else if (all(a$sak_beidz[c(1,4,6,8,10,11)] == "1") && 
               all(sapply(c(1,2,4,6,8,10), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) != 0)) && 
               all(sapply(seq(3,9,by=2), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) == 0))) {
                a1 <- a[11, ]; a10 <- a[-11,]
                if (kods %in% c("40", "50", "53") && o == "11") {ZERO_minus(a %>% slice(1))}
    } else if (all(a$sak_beidz[c(1,4,5,7,9,11)] == "1") &&
               all(sapply(c(2,4,6,8,10), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) != 0)) && 
               all(sapply(c(1,3,7), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) == 0))) {
               a1 <- a[11, ]; a10 <- a[-11,]
               if (kods %in% c("40", "50", "53") && o == "11") {ZERO_minus(a %>% slice(1))}
    } else if (all(a$sak_beidz[c(2,4,6,8,10,11)] == "1") && all(diff(a$NDZ_sanemsanas_datums) != 0)) {
               a1 <- a[11, ]; a9 <- a[1:9,]
    } else if (all(a$sak_beidz[seq(1,11,by=2)] == "1")) { 
              if (all(diff(a$NDZ_sanemsanas_datums) != 0)) {
                       a1 <- a[11, ]; a10 <- a[-11,]
                       if (kods %in% c("40", "50", "53") && o == "11") {ZERO_minus(a %>% slice(1))}
              } else if (all(sapply(c(1:6,8:10), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) != 0)) &&
                         diff(a$NDZ_sanemsanas_datums[7:8]) == 0) {
                       a1 <- a[11, ]; a10 <- a[-11,]
                       if (kods %in% c("40", "50", "53") && o == "11") {ZERO_minus(a %>% slice(1))}
              } else if (all(sapply(c(2,4:10), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) != 0)) &&
                         all(sapply(c(1,3), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) == 0))) {
                        a1 <- a[11, ]; a10 <- a[-11,]
                        if (kods %in% c("40", "50", "53") && o == "11") {ZERO_minus(a %>% slice(1))}
              } else {stop("processingEleven: Vienpadsmitnieku tabulas pārdalei trūkst izstrādes koda. Rindas: ", r, " līdz ", r + 10, "\n")}
    } else if (all(a$sak_beidz[c(2,3,5,7,9,11)] == "1")) {
              if (all(sapply(c(2,4,5,6,7,8,9,10), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) != 0)) &&
               all(sapply(c(1,3), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) == 0))) {
                a <- a[c(2,1,3,4,5,6,7,8,9,10,11),]
                a1 <- a[11, ]; a10 <- a[-11,]
               if (kods %in% c("40", "50", "53") && o == "11") {ZERO_minus(a %>% slice(1))}
              } else if (all(diff(a$NDZ_sanemsanas_datums[2:11]) != 0) &&
                         diff(a$NDZ_sanemsanas_datums[1:2]) == 0) {
                          if (a$period[1] == "_____" && a$PS_code[1] == "__________" && a$NM_code[1] == "__________") {
                            a <- a[c(2,1,3,4,5,6,7,8,9,10,11),]
                            a1 <- a[11, ]; a10 <- a[-11,]
                            if (kods %in% c("40", "50", "53") && o == "11") {ZERO_minus(a %>% slice(1))}
                          } else {stop("processingEleven: Vienpadsmitnieku tabulas pārdalei trūkst izstrādes koda. Rindas: ", r, " līdz ", r + 10, "\n")}
              } else {stop("processingEleven: Vienpadsmitnieku tabulas pārdalei trūkst izstrādes koda. Rindas: ", r, " līdz ", r + 10, "\n")}
    } else if (all(a$sak_beidz[c(2,3,5,7,10,11)] == "1") && 
               all(sapply(c(2,4,5,6,7,8,10), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) != 0)) &&
               all(sapply(c(1,3,9), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) == 0))) {
               a <- a[c(2,1,3,4,5,6,7,8,10,9,11),]; a1 <- a[11, ]
                a10 <- rbind(a10, a[-11,])
                if (kods %in% c("40", "50", "53") && o == "11") {ZERO_minus(a %>% slice(1))}
    } else if (all(a$sak_beidz[1:4] == c("1", "2", "1", "2")) && 
               diff(a$NDZ_sanemsanas_datums[1:2]) == 0 &&
               all(diff(a$NDZ_sanemsanas_datums[2:4]) != 0)) {
               a2 <- a[1:2, ]; a9 <- a[-(1:2),]
                if (kods %in% c("40", "50", "53") && o == "11") {ZERO_minus(a %>% slice(1))}
    } else if (all(a$sak_beidz[1:4] == c("2", "1", "2", "1"))) {
              if (all(diff(a$NDZ_sanemsanas_datums[1:4]) != 0)) {
                if (a$period[1] == "_____" && a$PS_code[1] == "__________" && a$NM_code[1] == "__________") {
                  a1 <- a[1, ]; a10 <- a[-1,]
                } else {stop("processingEleven_s6 trūkst izstrādes koda. \n")}
              } else {stop("processingEleven_s6 trūkst izstrādes koda. \n")}
    } else if (all(a$sak_beidz[1:4] == c("1", "2", "2", "1"))) {
              if (all(sapply(c(1,2,4,5,6,8,9,10), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) != 0)) &&
                  all(sapply(c(3,7), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) == 0))) {
                    if (a$period[1] == "_____" && a$PS_code[1] == "__________" && a$NM_code[1] == "__________") {
                    a2 <- a[1:2, ]; a9 <- a[3:11,]
                } else {stop("processingEleven_s6 trūkst izstrādes koda. \n")}
              } else {stop("processingEleven_s6 trūkst izstrādes koda. \n")}
    } else {stop("processingEleven_s6 trūkst izstrādes koda. \n")}
  class(a)
  rm(a, o, kods)
  return(list(x11_uzVieniniekiem = a1,
              x11_2 = a2,
              x11_uzDevini = a9,
              x11_uzDesmitniekiem = a10))
}

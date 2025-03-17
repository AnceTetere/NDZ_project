processingTens_s6 <- function(x10s6, o, kods) {
  x10s6 <- arrange(x10s6, PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
  x10s6_uzVieniniekiem <- data.frame(); x10s6_uzDivniekiem <- data.frame(); x10s6_uzSeptini <- data.frame(); x10s6_uzAstoniekiem <- data.frame()
  
  if (all(x10s6$sak_beidz[c(2, 4, 7, 9)] == "2") && all(x10s6$sak_beidz[c(1, 3, 5, 6, 8, 10)] == "1") &&
      all(sapply(c(1:3, 5:9), function(i) diff(x10s6$NDZ_sanemsanas_datums[i:(i+1)]) != 0)) && diff(x10s6$NDZ_sanemsanas_datums[4:5]) == 0) {
               x10s6_uzVieniniekiem <- x10s6[10, ]; x10s6_uzDivniekiem <- x10s6[1:2, ]; x10s6_uzSeptini <- x10s6[3:9, ]
                if (kods %in% c("40", "50", "53") && o == "10") {ZERO_minus(x10s6 %>% slice(1))}
  } else if (all(x10s6$sak_beidz[c(1,2,4,6,8,10)] == "1") && all(diff(x10s6$NDZ_sanemsanas_datums) != 0)) {
                 x10s6_uzVieniniekiem <- x10s6[10, ]; x10s6_uzAstoniekiem <- x10s6[2:9, ]
                 if (kods %in% c("40", "50", "53") && o == "10") {ZERO_minus(x10s6 %>% slice(2))}
  } else if (all(x10s6$sak_beidz[c(3:5,8:10)] == "1") && 
             all(sapply(c(1,3,6,8), function(i) diff(x10s6$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
             all(sapply(c(2,4,5,7,9), function(i) diff(x10s6$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
                 x10s6_uzVieniniekiem <- x10s6[c(1,10), ]; x10s6_uzDivniekiem <- x10s6[5:6, ]
  } else if (all(x10s6$sak_beidz[c(1,2,5,6,9, 10)] == "1")) {
                 if (all(sapply(seq(1,10,by=2), function(i) diff(x10s6$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
                            all(sapply(c(2,4,6,8), function(i) diff(x10s6$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
                      if (kods == "40") {
                          a <- x10s6 %>% dplyr::filter(substr(x10s6$zinkod,1,1) == "4")
                          if (all(a$sak_beidz == c("1", "2", "1", "2", "1"))) {
                            x10s6_uzVieniniekiem <- a[5, ]; x10s6_uzDivniekiem <- a[1:4, ]
                      } else {stop("processingTens_s6: Trūkst izstrādes koda.")}
                      if (kods %in% c("40", "50", "53") && o == "10") {ZERO_minus(x10s6 %>% slice(1:2))}
                          rm(a)
                  } else {stop("processingTens_s6: Trūkst izstrādes koda.")}
            } else {stop("processingTens_s6: Trūkst izstrādes koda.")}
  } else if (all(x10s6$sak_beidz[c(1,3,5,7,8,10)] == "1")) {
            if (all(diff(x10s6$NDZ_sanemsanas_datums) != 0)) {
              if (x10s6$period[1] == "_____" && x10s6$PS_code[1] == "__________" && x10s6$NM_code[1] == "__________") {
                 x10s6_uzDivniekiem <- x10s6[1:2,]; x10s6_uzAstoniekiem <- x10s6[-(1:2), ]
                if (kods %in% c("40", "50", "53") && o == "10") {ZERO_minus(x10s6 %>% slice(1))}
              } else {stop("processingTens_s6: Trūkst izstrādes koda.")}
            } else {stop("processingTens_s6: Trūkst izstrādes koda.")}
  } else {stop("processingTens_s6: Trūkst izstrādes koda.")}
  
  rm(x10s6, o, kods)
  return(list(x10_uzVieniniekiem = x10s6_uzVieniniekiem,
              x10_uzDivniekiem = x10s6_uzDivniekiem,
              x10_uzSeptini = x10s6_uzSeptini,
              x10_uzAstoniekiem = x10s6_uzAstoniekiem))
}

processingTens_s5e1 <- function(a, o, kods) {
  
  a1 <- data.frame(); a2 <- data.frame(); a5 <- data.frame(); a7 <- data.frame(); a8 <- data.frame()
 
 if (a$sak_beidz[3] != "2") {
    if (all(diff(a$NDZ_sanemsanas_datums[1:3]) != 0)) {
      a2 <- rbind(a2, a[1:2, ])
      a8 <- rbind(a8, a[3:10, ])
      if (kods %in% c("40", "50", "53") && o == "10") {ZERO_minus(a %>% slice(1))}
    } else if (diff(a$NDZ_sanemsanas_datums[1:2]) == 0 && diff(a$NDZ_sanemsanas_datums[2:3]) != 0) {
      if ((a$period[1] == "_______" && a$PS_code[1] == "_________" && a$nmrkod[1] == "_________") ||
          (a$period[1] == "_________" && a$PS_code[1] == "_________" && a$nmrkod[1] == "_________") ||
          (a$period[1] == "_________" && a$PS_code[1] == "_________" && a$nmrkod[1] == "_________") ||
          (a$period[1] == "_________" && a$PS_code[1] == "_________" && a$nmrkod[1] == "_________") ||
          (a$period[1] == "_________" && a$PS_code[1] == "_________" && a$nmrkod[1] == "_________")) {
        a2 <- rbind(a2, a[1:2, ])
        a8 <- rbind(a8, a[3:10, ])
      } else {stop("processingTens_s5: Desmitnieku tabulas pārdalei trūkst izstrādes koda\n")}
      if (kods %in% c("40", "50", "53") && o == "10") {ZERO_minus(a %>% slice(1))}
    } else {stop("processingTens_s5: Desmitnieku tabulas pārdalei trūkst izstrādes koda\n")}
  } else if (all(a$sak_beidz[3:4] == c("2", "1"))) {
    if (all(sapply(c(1,2,4), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) != 0)) && diff(a$NDZ_sanemsanas_datums[3:4]) == 0) {
      if ((a$period[1] == "_________" && a$PS_code[1] == "_________" && a$nmrkod[1] == "_________") ||
          (a$period[1] == "_________" && a$PS_code[1] == "_________" && a$nmrkod[1] == "_________") ||
          (a$period[1] == "_________" && a$PS_code[1] == "_________" && a$nmrkod[1] == "_________")) {
        a2 <- rbind(a2, a[1:2, ])
        a8 <- rbind(a8, a[c(4,3,5:10), ])
      } else {stop("processingTens_s5: Desmitnieku tabulas pārdalei trūkst izstrādes koda\n")}
      if (kods %in% c("40", "50", "53") && o == "10") {ZERO_minus(a %>% slice(1))}
    } else if (all(sapply(c(1,3), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) && 
               all(sapply(c(2,4), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
      if (a$period[1] == "_________" && a$PS_code[1] == "_________" && a$nmrkod[1] == "_________") {
        a2 <- rbind(a2, a[1:2, ])
        a8 <- rbind(a8, a[c(4,3,5:10), ])
      } else {stop("processingTens_s5: Desmitnieku tabulas pārdalei trūkst izstrādes koda\n")}
      if (kods %in% c("40", "50", "53") && o == "10") {ZERO_minus(a %>% slice(1))}
    } else {stop("processingTens_s5: Desmitnieku tabulas pārdalei trūkst izstrādes koda\n")}
  } else {stop("processingTens_s5: Desmitnieku tabulas pārdalei trūkst izstrādes koda\n")}
}

rm(a, o, kods)

return(list(x10s5_1 = a1, 
            x10s5_2 = a2, 
            x10s5_5 = a5, 
            x10s5_7 = a7,
            x10s5_8 = a8))
}

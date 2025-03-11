processingTwelve_s6 <- function(a, o, kods) {
  
  a1 <- data.frame(); a2 <- data.frame(); a7 <- data.frame(); a8 <- data.frame(); a10 <- data.frame(); a11 <- data.frame()
  #a <- x12
  
  if (all(a$sak_beidz[1:3] == c("2", "1", "2")) && diff(a$NDZ_sanemsanas_datums[1:2]) != 0) {
    a1 <- a[1, ]; a11 <- a[2:12, ]
  } else if(all(a$sak_beidz[1:3] == c("1", "2", "1")) && diff(a$NDZ_sanemsanas_datums[2:3]) != 0) {
    a2 <- a[1:2, ]; a10 <- a[-(1:2), ]
  } else if (all(a$sak_beidz[1:2] == c("2", "1")) && diff(a$NDZ_sanemsanas_datums[1:2]) == 0) {
    a2 <- a[1:2, ]; a10 <- a[-(1:2), ]
  } else if (all(a$sak_beidz[1:2] == c("1", "2"))) {
        if (a$sak_beidz[3] != "2") {
          if (diff(a$NDZ_sanemsanas_datums[1:2]) != 0) {
            a2 <- rbind(a2, a[1:2, ])
            a10 <- rbind(a10, a[-(1:2), ])
          } else {stop("12-nieku tabulas pārdalei trūkst izstrādes koda.")}
        } else if (a$sak_beidz[3] == "2") {
          if (all(a$sak_beidz[4:5] == c("1", "2"))) {
            if (all(diff(a$NDZ_sanemsanas_datums[1:4]) != 0)) {
              a2 <- rbind(a2, a[c(1,3), ])
              a9 <- rbind(a9, a[4:12, ])
            } else if (diff(a$NDZ_sanemsanas_datums[1:2]) == 0 && diff(a$NDZ_sanemsanas_datums[2:3]) != 0) {
              if ((a$period[1] == "_____" && a$PS_code[1] == "_____" && a$NM_code[1] == "_____") ||
                  (a$period[1] == "_____" && a$PS_code[1] == "_____" && a$NM_code[1] == "_____")) {
                    a2 <- rbind(a2, a[1:2, ])
                    a10 <- rbind(a10, a[3:12, ])
                } else {stop("12-nieku tabulas pārdalei trūkst izstrādes koda.")}
            } else if (all(diff(a$NDZ_sanemsanas_datums[1:3]) != 0) && diff(a$NDZ_sanemsanas_datums[3:4]) == 0) {
              if ((a$period[1] == "_____" && a$PS_code[1] == "_____" && a$NM_code[1] == "_____")) {
                    a2 <- rbind(a2, a[1:2, ])
                    a10 <- rbind(a10, a[3:12, ])
              } else {stop("12-nieku tabulas pārdalei trūkst izstrādes koda.")}
            } else {stop("12-nieku tabulas pārdalei trūkst izstrādes koda.")}
      } else if (all(a$sak_beidz[4:6] == c("1", "1", "2"))) {
            if (all(sapply(c(1,3,5), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) && 
                all(sapply(c(2,4), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
                #BLOĶĒJU, JO PIRMOREIZ
                  if ((a$period[1] == "_____" && a$PS_code[1] == "_____" && a$NM_code[1] == "_____") ||
                      (a$period[1] == "_____" && a$PS_code[1] == "_____" && a$NM_code[1] == "_____")) {
                    a2 <- a[1:2, ]; a10 <- a[c(4,3, 5:12), ]
                  } else {stop("12-nieku tabulas pārdalei trūkst izstrādes koda.")}
            } else if (diff(a$NDZ_sanemsanas_datums[9:10]) == 0 && 
                       all(sapply(c(1:8,10,11), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
                          if (a$period[1] == "_____" && a$PS_code[1] == "_____" && a$NM_code[1] == "_____") {
                          a2 <- rbind(a2, a[c(1,3), ])
                          a8 <- rbind(a8, a[c(5:12), ])
                         } else {stop("12-nieku tabulas pārdalei trūkst izstrādes koda.")}
            } else {stop("12-nieku tabulas pārdalei trūkst izstrādes koda.")}
      } else {stop("12-nieku tabulas pārdalei trūkst izstrādes koda.")}
    } else {stop("12-nieku tabulas pārdalei trūkst izstrādes koda.")}
    if (kods %in% c("40", "50", "53") && o == "12") {ZERO_minus(a %>% slice(1))}
  } else if (all(a$sak_beidz[c(1,2,4)] == "2") && all(a$sak_beidz[c(3, 5)] == "1") && 
             diff(a$NDZ_sanemsanas_datums[1:2]) != 0 &&
             all(sapply(seq(2, 5, by = 2), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) == 0))) {
    a1 <- rbind(a1, a[1, ])
    a11 <- rbind(a11, a[-1, ])
  } else if (all(a$sak_beidz[c(3, 4, 6, 8, 10, 12)] == "1") && 
             all(sapply(c(1, 5, 6, 9, 10), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
             all(sapply(c(2, 3, 4, 7, 8, 11), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
    a1 <- rbind(a1, a[1, ])
    a7 <- rbind(a7, a[c(4, 7:12), ])
  } else if (all(sapply(seq(1,12,by=2), function(i) a$sak_beidz[i] == a$sak_beidz[i+1]))) {
    if (all(sapply(seq(1,12,by=2), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
        all(sapply(seq(2,11,by=2), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
      if ("92" %in% a$zinkod) {
        a1 <- rbind(a1, a[c(1,12), ])
        a2 <- rbind(a2, a[c(3,5,7,9), ])
      } else {stop("12-nieku tabulas pārdalei trūkst izstrādes koda.")}
    } else {stop("12-nieku tabulas pārdalei trūkst izstrādes koda. Rindas:",r, " līdz ", r + 11)}
  } else {stop("12-nieku tabulas pārdalei trūkst izstrādes koda. Rindas:",r, " līdz ", r + 11)} 
  
  rm(a, o, kods)
  
  return(list(x12_uzVieniniekiem = a1,
              x12_uzDivniekiem = a2,
              x12_uzSeptini = a7,
              x12_uzAstoni = a8,
              x12_uzDesmitniekiem = a10,
              x12_uzVienpadsmit = a11))
}

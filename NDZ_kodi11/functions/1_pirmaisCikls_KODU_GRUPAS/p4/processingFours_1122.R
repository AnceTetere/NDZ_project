processingFours_1122 <- function(a, o, kods) {
  a4_1 <- data.frame(); a4_2 <- data.frame(); a4_3 <- data.frame()
  #a <- x4
  if (diff(a$NDZ_sanemsanas_datums[2:3]) == 0 && 
      all(sapply(c(1,3), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) { 
        if (a$period[1] == "_____" && a$PS_code[1] == '_____' && a$NM_code[1] == '_____') {
          a4_2 <- a[c(2,4), ]
          if (kods %in% c("40", "50", "53")) {ZERO_minus(a %>% slice(2)); ZERO_plus(a %>% slice(4))}
      } else if ((a$PS_code[1] == '_____' && a$NM_code[1] == '_____') ||
          (a$PS_code[1] %in% c('_____', '_____', '_____') && a$NM_code[1] == _____')) {
      a4_2 <- rbind(a4_2, a[c(1,3,2,4), ])
      if (kods %in% c("40", "50", "53")) {ZERO_minus(a %>% slice(1)); ZERO_plus(a %>% slice(4))}
      } else {stop("processingFours_1122 trūkst apstrādes koda.")}
  } else if (all(diff(a$NDZ_sanemsanas_datums) != 0)) {
          if (a$period[1] == "_____" && a$PS_code[1] %in% c('_____', '_____') && a$NM_code[1] == '_____') {
            a4_2 <- a[c(2,4), ]
            if (kods %in% c("40", "50", "53")) {ZERO_minus(a %>% slice(1)); ZERO_plus(a %>% slice(4))}
          } else {stop("processingFours_1122 trūkst apstrādes koda.")}
  } else if (all(sapply(c(1,3), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
             diff(a$NDZ_sanemsanas_datums[2:3]) != 0) {
              if (a$period[1] == "_____" && a$PS_code[1] == '_____' && a$NM_code[1] == '_____') {
               a4_2 <- a[c(2,4), ]
              if (kods %in% c("40", "50", "53") && o == "4") {ZERO_minus(a %>% slice(1:2)); ZERO_plus(a %>% slice(3:4))}
    } else {stop("processingFours_1122 trūkst apstrādes koda.")}
  } else {stop("processingFours_1122 trūkst apstrādes koda.")}

  return(list(x4_uzVieniniekiem = a4_1,
              x4_trueDoubles = a4_2,
              x4_uzTrijniekiem= a4_3))
}

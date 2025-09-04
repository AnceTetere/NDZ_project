processingFours_2212 <- function(a, o, kods) {
  a4_1 <- data.frame(); a4_2 <- data.frame(); a4_3 <- data.frame()
  #a <- x4
  
  if (diff(a$NDZ_sanemsanas_datums[1:2]) > 0 && diff(a$NDZ_sanemsanas_datums[2:3]) == 0) {
    a4_1 <- rbind(a4_1, a[1, ])
    a4_2 <- rbind(a4_2, a[3:4, ])
    if (kods %in% c("40", "50", "53") & o == "4") {ZERO_plus(a %>% slice(4))}
  } else if (all(diff(a$NDZ_sanemsanas_datums) != 0)) {
            a4_1 <- rbind(a4_1, a[2, ])
            a4_2 <- rbind(a4_2, a[3:4, ])
            if (kods %in% c("40", "50", "53") & o == "4") {ZERO_plus(a %>% slice(4))}
  } else if (diff(a$NDZ_sanemsanas_datums[1:2]) == 0 && all(diff(a$NDZ_sanemsanas_datums[2:4]) != 0)) {
            #JO PIRMOREIZ
            if (kods == "40" && a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') {
              a4_1 <- a[2, ]; a4_2 <- a[3:4, ]
              if (kods %in% c("40", "50", "53") & o == "4") {ZERO_plus(a %>% slice(4))}
            } else {stop("processingFours_2212 trūkst apstrādes koda.")}
  } else {stop("processingFours_2212 trūkst apstrādes koda.")}
  
  return(list(x4_uzVieniniekiem = a4_1,
              x4_trueDoubles = a4_2,
              x4_uzTrijniekiem = a4_3))
}

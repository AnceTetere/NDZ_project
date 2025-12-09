processingSeven_s4_1356 <- function(a, o, kods) {
  #a <- x7s4
  a1 <- data.frame(); a2 <- data.frame(); a3 <- data.frame(); a4 <- data.frame(); a5 <- data.frame(); a6 <- data.frame()
  
  if (all(diff(a$NDZ_sanemsanas_datums) != 0)) {
          a6 <- a[c(1,2,3,4,6,7), ]
          if (kods %in% c("40", "50", "53") && o == "7") {ZERO_plus(a %>% slice(7)); ZERO_minus(a %>% slice(1))}
  } else if (all(sapply(c(1,3,5), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) != 0)) &&
             all(sapply(c(2,4,6), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) == 0))) {
             #JO PIRMOREIZ UN TĀ KOMPĀNIJA
             if ((a$period[1] == '202203' && a$PS_code[1] == '______________' && a$NM_code[1] == '______________') ||
                 (a$period[1] == '202203' && a$PS_code[1] == '______________' && a$NM_code[1] == '______________') ||
                 (a$period[1] == '202203' && a$PS_code[1] == '______________' && a$NM_code[1] == '______________') ||
                 (a$period[1] == '202203' && a$PS_code[1] == '______________' && a$NM_code[1] == '______________') ||
                 (a$period[1] == '202203' && a$PS_code[1] == '______________' && a$NM_code[1] == '______________')) {
                  a1 <- a[6,]; a6 <- a[c(1,2,3,4,5,7), ]
                  if (kods %in% c("40", "50", "53") && o == "7") {ZERO_minus(a %>% slice(1))}
             } else {stop("processingSeven_s4_1356 trūkst izstrādes koda.\n")}
  } else if (all(diff(a$NDZ_sanemsanas_datums[2:6]) != 0) &&
             all(sapply(c(1,6), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) == 0))) {
              #JO PIRMOREIZ
              if (a$period[1] == '202207' && a$PS_code[1] == '______________' && a$NM_code[1] == '______________') {
                  a1 <- a[6]; a6 <- a[c(1,2,3,4,5,7), ]
                  if (kods %in% c("40", "50", "53") && o == "7") {ZERO_minus(a %>% slice(1))}
              } else {stop("processingSeven_s4_1356 trūkst izstrādes koda.\n")}
  } else if (diff(a$NDZ_sanemsanas_datums[4:5]) == 0 &&
             all(sapply(c(1,2,3,5,6), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
             #JO PIRMOREIZ
             if (a$period[1] == '202208' && a$PS_code[1] == '______________' && a$NM_code[1] == '______________') {
                 a6 <- a[c(1,2,3,4,6,7), ]
                 if (kods %in% c("40", "50", "53") && o == "7") {ZERO_minus(a %>% slice(1)); ZERO_plus(a %>% slice(7))}
            } else {stop("processingSeven_s4_1356 trūkst izstrādes koda.\n")}
 # } else if (all(sapply(c(1,2,3,5), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) ||
#             all(sapply(c(4,6), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
#             #JO PIRMOREIZ
#             if (a$period[1] == '202202' && a$PS_code[1] == '______________' && a$NM_code[1] == '______________') {
#      a6 <- a[c(1,2,3,4,6,7), ]
#      if (kods %in% c("40", "50", "53") && o == "7") {ZERO_minus(a %>% slice(1)); ZERO_plus(a %>% slice(7))}
#    } else {stop("processingSeven_s4_1356 trūkst izstrādes koda.\n")}
  } else {stop("processingSeven_s4_1356 trūkst izstrādes koda.\n")}

  
  rm(a, o, kods)
  return(list(x7s4_uzVieniniekiem = a1,
              x7s4_uzDivniekiem = a2,
              x7s4_uzTrijniekiem = a3,
              x7s4_uzCetriniekiem = a4, 
              x7s4_uzPieciniekiem = a5,
              x7s4_uzSesiniekiem = a6))
}   

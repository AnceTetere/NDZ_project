processingSeven_s4_2467 <- function(a, o, kods) {
  
  a1 <- data.frame(); a2 <- data.frame(); a3 <- data.frame(); a4 <- data.frame(); a5 <- data.frame(); a6 <- data.frame()
  #a <- x7s4
  
  if (all(sapply(c(1,2,3,4,6), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) != 0)) &&
      diff(a$NDZ_sanemsanas_datums[5:6]) == 0) {
        #It kā viss OK, bet neesmu vēl pārliecināta, ka šo var vispārināt.
        if (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') {
          a1 <- a[c(1,7),]; a4 <- a[c(2,3,4,5), ]
        } else {stop("processingSeven_s4_2467 trūkst izstrādes koda.\n")}
  } else if (all(diff(a$NDZ_sanemsanas_datums) != 0)) {
              #It kā viss OK, bet neesmu vēl pārliecināta, ka šo var vispārināt.
              if ((a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') ||
                  (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') ||
                  (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') ||
                  (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________')) {
                     a1 <- a[c(1,7),]; a4 <- a[c(2,3,4,5), ]
              } else {stop("processingSeven_s4_2467 trūkst izstrādes koda.\n")}
  } else if (all(sapply(seq(1,6,by=2), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
             all(sapply(seq(2,6,by=2), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
                #It kā viss OK, bet neesmu vēl pārliecināta, ka šo var vispārināt.
                if ((a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') ||
                    (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') ||
                    (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________')) {
                     a <- a[c(2,1,4,3,6,5,7),]; a1 <- a[7,]; a6 <- a[1:6, ]
                     if (kods %in% c("40", "50", "53") && o == "7") {ZERO_minus(a %>% slice(1))}
                } else {stop("processingSeven_s4_2467 trūkst izstrādes koda.\n")}
  } else {stop("processingSeven_s4_2467 trūkst izstrādes koda.\n")}
  
  rm(a, o, kods)
  return(list(x7s4_uzVieniniekiem = a1,
              x7s4_uzDivniekiem = a2,
              x7s4_uzTrijniekiem = a3,
              x7s4_uzCetriniekiem = a4, 
              x7s4_uzPieciniekiem = a5,
              x7s4_uzSesiniekiem = a6))
}

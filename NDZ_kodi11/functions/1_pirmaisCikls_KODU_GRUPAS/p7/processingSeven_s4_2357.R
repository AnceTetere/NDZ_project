processingSeven_s4_2357 <- function(a, o, kods) {
  
  a1 <- data.frame(); a2 <- data.frame(); a3 <- data.frame(); a4 <- data.frame(); a5 <- data.frame(); a6 <- data.frame()
  #a <- x7s4 
  if (all(diff(a$NDZ_sanemsanas_datums) != 0)) {
            a4 <- a[3:6, ]; a1 <- a[c(1,7), ]
  } else if (all(sapply(2:6, function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) != 0)) &&  diff(a$NDZ_sanemsanas_datums[1:2]) == 0) {
            a2 <- a[c(2,1,3,4,5,6), ]; a1 <- a[7, ]
  } else if (all(sapply(c(1,2,4,5,6), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) != 0)) && diff(a$NDZ_sanemsanas_datums[3:4]) == 0) {
            if ((a$PS_code[1] == '__________' && a$NM_code[1] == '__________') ||
                (a$period[1] == '_____' && a$PS_code[1] == '__________' && a$NM_code[1] == '__________') ||
                (a$period[1] == '_____' && a$PS_code[1] == '__________' && a$NM_code[1] == '__________')) {
                  a2 <- a[c(3:6), ]; a1 <- a[c(1,7), ]
            } else {stop("processingSeven_s4 trūkst izstrādes koda.\n")}
  } else if (all(sapply(c(1,5), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) && 
             all(sapply(c(2,3,4,6), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
            if (a$period[1] == "_____" && a$PS_code[1] == 'PK7C1D87E55' && a$NM_code[1] == '__________') {
             a2 <- a[c(2,1,3,4,5,6), ]; a1 <- a[7, ]
    } else {stop("processingSeven_s4 trūkst izstrādes koda.\n")}
  } else {stop("processingSeven_s4 trūkst izstrādes koda.\n")}
  
  rm(a, o, kods)
  return(list(x7s4_uzVieniniekiem = a1,
              x7s4_uzDivniekiem = a2,
              x7s4_uzTrijniekiem = a3,
              x7s4_uzCetriniekiem = a4, 
              x7s4_uzPieciniekiem = a5,
              x7s4_uzSesiniekiem = a6))
}

processingSeven_s4_2467 <- function(a, o, kods) {
  
  a1 <- data.frame(); a2 <- data.frame(); a3 <- data.frame(); a4 <- data.frame(); a5 <- data.frame(); a6 <- data.frame()
  
  
  if (all(sapply(c(1,2,3,4,6), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) != 0)) &&
      diff(a$NDZ_sanemsanas_datums[5:6]) == 0) {
        if (a$period[1] == "______" && a$PS_code[1] == '______' && a$NM_code[1] == '______') {
          a1 <- rbind(a1, a[c(1,7),])
          a4 <- rbind(a4, a[c(2,3,4,5), ])
        } else {stop("processingSeven_s4_2467 trūkst izstrādes koda.\n")}
  } else if (all(diff(a$NDZ_sanemsanas_datums) != 0)) {
        if ((a$period[1] == "______" && a$PS_code[1] == '______' && a$NM_code[1] == '______') ||
            (a$period[1] == "______" && a$PS_code[1] == '______' && a$NM_code[1] == '______')) {
          a1 <- rbind(a1, a[c(1,7),])
          a4 <- rbind(a4, a[c(2,3,4,5), ])
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

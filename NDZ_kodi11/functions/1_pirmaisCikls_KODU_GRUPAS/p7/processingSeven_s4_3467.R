processingSeven_s4_3467 <- function(a, o, kods) {
  a1 <- data.frame(); a2 <- data.frame(); a3 <- data.frame(); a4 <- data.frame(); a5 <- data.frame(); a6 <- data.frame()
  #a <- x7s4
  
  if (all(diff(a$NDZ_sanemsanas_datums[2:7]) != 0) && diff(a$NDZ_sanemsanas_datums[1:2]) == 0) {
      #It kā viss OK, bet neesmu vēl pārliecināta, ka šo var vispārināt.
      if (kods == "40" && a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') {
          a1 <- a[c(1,7),]; a2 <- a[4:5, ]
      } else {stop("processingSeven_s4_2567 trūkst izstrādes koda.\n")}
  } else {stop("processingSeven_s4_2567 trūkst izstrādes koda.\n")}
  
  rm(a, o, kods)
  return(list(x7s4_uzVieniniekiem = a1,
              x7s4_uzDivniekiem = a2,
              x7s4_uzTrijniekiem = a3,
              x7s4_uzCetriniekiem = a4, 
              x7s4_uzPieciniekiem = a5,
              x7s4_uzSesiniekiem = a6))
}

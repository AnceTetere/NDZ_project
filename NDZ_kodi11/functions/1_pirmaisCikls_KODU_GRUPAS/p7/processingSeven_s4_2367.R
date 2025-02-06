processingSeven_s4_2367 <- function(a, o, kods) {
  
  a1 <- data.frame(); a2 <- data.frame(); a3 <- data.frame(); a4 <- data.frame(); a5 <- data.frame(); a6 <- data.frame()
  
  if (all(sapply(c(1,3,5), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
      all(sapply(c(2,4,6), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
    if (a$period[1] == "______" && a$pseidokods[1] == "______" && a$nmrkod[1] == "______") {
      a6 <- rbind(a6, a[c(2,1,3,4,6,5), ])
      a1 <- rbind(a1, a[7, ])
      if (kods %in% c("40", "50", "53") && o == "7") {ZERO_minus(a %>% slice(2))}
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

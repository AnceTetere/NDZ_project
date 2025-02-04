processingSeven_s4_1345 <- function(a, o, kods) {
  
  a1 <- data.frame(); a2 <- data.frame(); a3 <- data.frame(); a4 <- data.frame(); a5 <- data.frame(); a6 <- data.frame()
  
  if (all(sapply(c(1,3:6), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
    a3 <- rbind(a3, a[1:3, ])
    a4 <- rbind(a4, a[4:7, ])      
  } else if (all(sapply(c(1,2,3,5), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) != 0)) &&
             all(sapply(c(4,6), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) == 0))) {
    a1 <- rbind(a1, a[6, ])
    a6 <- rbind(a6, a[c(1,2,3,5,4,7), ])
    if (kods %in% c("40", "50", "53") && o == "7") {ZERO_plus(a %>% slice(7)); ZERO_minus(a %>% slice(1))}
  } else {stop("processingSeven trūkst izstrādes koda.\n")}
  
  
  return(list(x7s4_uzVieniniekiem = a1,
              x7s4_uzDivniekiem = a2,
              x7s4_uzTrijniekiem = a3,
              x7s4_uzCetriniekiem = a4, 
              x7s4_uzPieciniekiem = a5,
              x7s4_uzSesiniekiem = a6))
}

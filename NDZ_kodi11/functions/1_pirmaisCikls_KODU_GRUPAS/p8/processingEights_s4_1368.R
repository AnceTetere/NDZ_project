processingEights_s4_1368 <- function(a, o, kods) {

  #a <- x8s4
  a1 <- data.frame(); a2 <- data.frame(); a4 <- data.frame(); a6 <- data.frame(); a7 <- data.frame()
  
    if (all(diff(a$NDZ_sanemsanas_datums) != 0)) {
      if (a$period[1] == "_____" && a$PS_code[1] == "_____" && a$NM_code[1] == "_____") {
        a_uzVieniniekiem <- rbind(a_uzVieniniekiem, a[8, ]) 
        a_uzDivniekiem <- rbind(a_uzDivniekiem, a[c(1,2,3,4,6,7), ])
        if (kods %in% c("40", "50", "53") && o == "8") {ZERO_plus(a %>% slice(5))} 
      } else {stop("processingEights_s4: Trūkst izstrādes koda.")}
    } else if (diff(a$NDZ_sanemsanas_datums[3:4]) == 0 &&
               all(sapply(c(1,2,4,5,6,7), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
      if (a$period[1] == "_____" && a$PS_code[1] == "_____" && a$NM_code[1] == "_____") {
        a_uzVieniniekiem <- rbind(a_uzVieniniekiem, a[8, ]) 
        a_uzDivniekiem <- rbind(a_uzDivniekiem, a[c(1,2,3,5,6,7), ])
        if (kods %in% c("40", "50", "53") && o == "8") {ZERO_plus(a %>% slice(5))} 
      } else {stop("processingEights_s4: Trūkst izstrādes koda.")}
    } else if (all(sapply(c(3,5,7), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
               all(sapply(c(1,2,4,6), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
      if (a$period[1] == "_____" && a$PS_code[1] == "_____" && a$NM_code[1] == "_____") {
        a_uzDivniekiem <- a[c(1,2,3,4,6,5,8,7),]
        if (kods %in% c("40", "50", "53") && o == "8") {ZERO_minus(a %>% slice(1))} 
      } else {stop("processingEights_s4: Trūkst izstrādes koda.")}
    } else if (all(sapply(c(5,7), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
               all(sapply(c(1,2,3,4,6), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
      if ((a$period[1] == "_____" && a$PS_code[1] == "_____" && a$NM_code[1] == "_____") ||
          (a$period[1] == "_____" && a$PS_code[1] == "_____" && a$NM_code[1] == "_____")) {
        a_uzDivniekiem <- a[c(1,2,3,4,6,5,8,7),]
        if (kods %in% c("40", "50", "53") && o == "8") {ZERO_minus(a %>% slice(1)); ZERO_plus(a %>% slice(8))} 
      } else {stop("processingEights_s4: Trūkst izstrādes koda.")}
    } else {stop("processingEights_s4: Trūkst izstrādes koda.")}

  rm(a, o, kods)

  return(list(x8s4_uzVieniniekiem = a1, 
              x8s4_uzDivniekiem = a2, 
              x8s4_uzCetriniekiem = a4, 
              x8s4_uzSesi = a5, 
              x8s4_uzSeptini = a6))
}

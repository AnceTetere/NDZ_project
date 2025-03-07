processingEights_s5 <- function(x8s5, o, kods) {
  x8s5 <- x8s5 %>% arrange(PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
  x8s5_uzVieniniekiem <- data.frame(); x8s5_uzDivniekiem <- data.frame(); x8s5_uzCetriniekiem <- data.frame()
  x8s5_uzSesi <- data.frame(); x8s5_uzSeptini <- data.frame()
  
#  result <- function(x) {
#    x8s5_uzVieniniekiem <<- x$x8s5_uzVieniniekiem
#    x8s5_uzDivniekiem <<- x$x8s5_uzDivniekiem
#    x8s5_uzCetriniekiem <<- x$x8s5_uzCetriniekiem
#    x8s5_uzSesi <<- x$x8s5_uzSesi
#    x8s5_uzSeptini <<- x$x8s5_uzSeptini
#    rm(x)}
  
 if (all(x8s5$sak_beidz[c(2,3,4,6,8)] == "1")) {
      if (diff(x8s5$NDZ_sanemsanas_datums[1:2]) == 0 && all(diff(x8s5$NDZ_sanemsanas_datums[2:5]) != 0)) {
        if (x8s5$period[1] == "______" && x8s5$PS_code[1] == '______' && x8s5$NM_code[1] == '______') {
            x8s5_uzDivniekiem <- x8s5[c(2,1,4,5,6,7), ]
            x8s5_uzVieniniekiem <- x8s5[1, ]
            if (kods %in% c("40", "50", "53") && o == "8") {ZERO_minus(x8s5 %>% slice(2))} 
        } else {stop("processingEights_s5: Iztrūkst kods rindām ", r, " līdz ", r + 7,".\n")}
      } else {stop("processingEights_s5: Iztrūkst kods rindām ", r, " līdz ", r + 7,".\n")}
} else if(all(x8$sak_beidz[c(1,3,5,7,8)] == "1") && 
      all(sapply(c(3, 6), function(i) all(diff(x8s5$NDZ_sanemsanas_datums[i:(i+1)]) == 0))) &&
      all(sapply(c(1,2,4,5,7), function(i) all(diff(x8s5$NDZ_sanemsanas_datums[i:(i+1)]) != 0)))) {
        x8s5_uzVieniniekiem <- x8s5[8,]
        x8s5_uzSesi <- x8s5[1:6,]
        if (kods %in% c("40", "50", "53") && o == "8") {ZERO_minus(x8s5 %>% slice(1))} 
} else if (all(x8s5$sak_beidz[c(1, 3, 5, 6, 8)] == "1") && 
             all(diff(x8s5$NDZ_sanemsanas_datums) != 0)) {
                x8s5_uzVieniniekiem <- x8s5[8,]
                x8s5_uzSesi <- x8s5[c(1:4,6:7),]
                if (kods %in% c("40", "50", "53") && o == "8") {ZERO_minus(x8s5 %>% slice(1))} 
} else {stop("processingEights: Iztrūkst kods rindām ", r, " līdz ", r + 7,".\n")}
  
  rm(x8s5, o, kods)
  return(list(x8_uzVieniniekiem = x8s5_uzVieniniekiem, 
              x8_uzDivniekiem = x8s5_uzDivniekiem, 
              x8_uzCetriniekiem = x8s5_uzCetriniekiem, 
              x8_uzSesi = x8s5_uzSesi, 
              x8_uzSeptini = x8s5_uzSeptini))
}

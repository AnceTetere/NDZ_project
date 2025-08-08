processingEights_s6 <- function(x8s6, o, kods) {
  #x8s6 <- x8  
  x8s6 <- x8s6 %>% arrange(PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
  x8s6_uzVieniniekiem <- data.frame(); x8s6_uzDivniekiem <- data.frame(); x8s6_uzCetriniekiem <- data.frame()
  x8s6_uz5 <- data.frame(); x8s6_uzSesi <- data.frame(); x8s6_uzSeptini <- data.frame()
  
  if (all(x8s6$sak_beidz == c("1", "2", "1", "1", "2", "1", "1", "1")) && 
      all(diff(x8s6$NDZ_sanemsanas_datums[1:7]) != 0) && 
      diff(x8s6$NDZ_sanemsanas_datums[7:8]) == 0 &&
      x8s6$PS_code[1] ==  '______________' && x8s6$NM_code[1] ==  '______________') {
          x8s6_uzVieniniekiem <- rbind(x8s6_uzVieniniekiem, x8s6[1,])
  } else if (all(x8s6$sak_beidz[c(1,2,4,5,7,8)] == "1")) { 
          if (all(diff(x8s6$NDZ_sanemsanas_datums) != 0)) {
              
              if ((x8s6$period[1] == '______' && x8s6$PS_code[1] ==  '______________' && x8s6$NM_code[1] ==  '______________') ||
                  (x8s6$period[1] == '______' && x8s6$PS_code[1] ==  '______________' && x8s6$NM_code[1] ==  '______________')) {
                  x8s6_uzVieniniekiem <- x8s6[8,]; x8s6_uzCetriniekiem <- x8s6[c(2,3,5,6),]
                  if (kods %in% c("40", "50", "53") && o == "8") {ZERO_minus(x8s6 %>% slice(1))}
              } else {stop("processingEights_s6: Iztrūkst kods. \n")}
           } else if (all(sapply(c(1:5,7), function(i) diff(x8s6$NDZ_sanemsanas_datums[i:(i+1)]) != 0)) && 
                      diff(x8s6$NDZ_sanemsanas_datums[6:7]) == 0) {
                      
                      if (x8s6$period[1] == '______' && x8s6$PS_code[1] ==  '______________' && x8s6$NM_code[1] ==  '______________') {
                         x8s6_uzVieniniekiem <- x8s6[8,]; x8s6_uzCetriniekiem <- x8s6[c(2,3,5,6),]
                         if (kods %in% c("40", "50", "53") && o == "8") {ZERO_minus(x8s6 %>% slice(1))}
                      } else {stop("processingEights_s6: Iztrūkst kods. \n")}
            } else {stop("processingEights_s6: Iztrūkst kods. \n")} 
  } else {stop("processingEights_s6: Iztrūkst kods rindām ", r, " līdz ", r + 7,".\n")}
  
  
  rm(x8s6, o, kods)
  return(list(x8_uzVieniniekiem = x8s6_uzVieniniekiem, 
              x8_uzDivniekiem = x8s6_uzDivniekiem, 
              x8_uzCetriniekiem = x8s6_uzCetriniekiem, 
              x8_uz5 = x8s6_uz5, 
              x8_uzSesi = x8s6_uzSesi, 
              x8_uzSeptini = x8s6_uzSeptini))
}

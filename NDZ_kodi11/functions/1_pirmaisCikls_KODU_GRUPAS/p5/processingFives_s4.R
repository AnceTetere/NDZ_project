processingFives_s4 <- function(x5s4, o, kods) {
  x5s4 <- x5s4 %>% arrange(PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
  #x5s4 <- x5
  x5s4_uzVieniniekiem <- data.frame(); x5s4_uzDivniekiem <- data.frame(); x5s4_uzCetriniekiem <- data.frame()
  
  result <- function(y) {
    x5s4_uzVieniniekiem <<- rbind(x5s4_uzVieniniekiem, y$x5s4_uzVieniniekiem)
    x5s4_uzDivniekiem <<- rbind(x5s4_uzDivniekiem, y$x5s4_uzDivniekiem)
    x5s4_uzCetriniekiem <<- rbind(x5s4_uzCetriniekiem, y$x5s4_uzCetriniekiem)
    rm(y)
  }
  
   if (all(x5s4$sak_beidz[c(1,2,4,5)] == "1")) {
          if (all(diff(x5s4$NDZ_sanemsanas_datums) != 0)) {
            x5s4_uzVieniniekiem <- x5s4[5,]; x5s4_uzDivniekiem <- x5s4[2:3,]
          } else if (diff(x5s4$NDZ_sanemsanas_datums[3:4]) == 0 && all(sapply(c(1,2,4), function(i) diff(x5s4$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
            x5s4_uzVieniniekiem <- x5s4[5,]; x5s4_uzDivniekiem <- x5s4[2:3,]
          } else if (diff(x5s4$NDZ_sanemsanas_datums[2:3]) == 0 && all(sapply(c(1,3,4), function(i) diff(x5s4$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
              if (x5s4$period[1] == "_____" && x5s4$PS_code[1] == "_____" && x5s4$NM_code[1] == "_____") {
                x5s4_uzVieniniekiem <- x5s4[5,]; x5s4_uzDivniekiem <- x5s4[2:3,]
              } else {stop("processingFives iztrūkst koda.\n")}
          } else {stop("processingFives: Tabula nepārdalījās; rinda: ", r, ".\n")}
              if (kods %in% c("40", "50", "53") && o == "5") {ZERO_minus(x5s4 %>% slice(1))}
    } else if (all(x5s4$sak_beidz[1:4] == "1")) {
              x5s4_uzDivniekiem <- x5s4[4:5,]
              if (kods %in% c("40", "50", "53") && o == "5") {ZERO_plus(x5s4 %>% slice(5)); ZERO_minus(x5s4 %>% slice(1))}
    } else if (all(x5s4$sak_beidz[c(1:3,5)] == "1")) {
          if (all(sapply(c(1,2,4), function(i) diff(x5s4$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
            if (x5s4$PS_code[1] == '_____' && x5s4$NM_code[1] == '_____') {
              x5s4_uzVieniniekiem <- x5s4[5, ]; x5s4_uzDivniekiem <- x5s4[3:4,]
            } else {stop("processingFives: Iztrūkst apstrādes kods. \n")}
          } else {stop("processingFives: Iztrūkst apstrādes kods. \n")}
            if (kods %in% c("40", "50", "53") && o == "5") {ZERO_minus(x5s4 %>% slice(1))}
    } else {stop("processingFives: Tabula nepārdalījās; rinda: ", r, ".\n")}
    
  
  rm(x5s4, o, kods)
  return(list(x5_uzVieniniekiem = x5s4_uzVieniniekiem, 
              x5_uzDivniekiem = x5s4_uzDivniekiem, 
              x5_uzCetriniekiem = x5s4_uzCetriniekiem))
}

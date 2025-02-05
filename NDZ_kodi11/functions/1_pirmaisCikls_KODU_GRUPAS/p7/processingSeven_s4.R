processingSeven_s4 <- function(x7s4, o, kods) {
  
  x7s4_uzVieniniekiem <- data.frame(); x7s4_uzDivniekiem <- data.frame(); x7s4_uzTrijniekiem <- data.frame(); x7s4_uzCetriniekiem <- data.frame(); x7s4_uzPieciniekiem <- data.frame(); x7s4_uzSesiniekiem <- data.frame()
  
  result <- function(y) {
    x7s4_uzVieniniekiem <<- rbind(x7s4_uzVieniniekiem, y$x7s4_uzVieniniekiem)
    x7s4_uzDivniekiem   <<- rbind(x7s4_uzDivniekiem, y$x7s4_uzDivniekiem)
    x7s4_uzTrijniekiem  <<- rbind(x7s4_uzTrijniekiem, y$x7s4_uzTrijniekiem)
    x7s4_uzCetriniekiem <<- rbind(x7s4_uzCetriniekiem, y$x7s4_uzCetriniekiem)
    x7s4_uzPieciniekiem <<- rbind(x7s4_uzPieciniekiem, y$x7s4_uzPieciniekiem)
    x7s4_uzSesiniekiem  <<- rbind(x7s4_uzSesiniekiem, y$x7s4_uzSesiniekiem)
    rm(y)
  }
  
  if (all(x7s4$sak_beidz[c(1,3,4,5)] == "1")) {
           result(processingSeven_s4_1345(x7s4, o, kods))
  } else if (all(x7s4$sak_beidz[c(1,3,6,7)] == "1")) { 
            result(processingSeven_s4_1367(x7s4, o, kods))
  } else if (all(x7s4$sak_beidz[c(2,3,5,7)] == "1")) {
            result(processingSeven_s4_2357(x7s4, o, kods))
  } else if (all(x7s4$sak_beidz[c(1,3,5,7)] == "1")) {
            result(processingSeven_s4_1212121(x7s4, o, kods))
  } else if (all(x7s4$sak_beidz[c(1,2,4,7)] == "1")) {
            result(processingSeven_s4_1247(x7s4, o, kods))
  } else if (all(x7s4$sak_beidz[c(1,3,5,6)] == "1")) {
            result(processingSeven_s4_1356(x7s4, o, kods))
  } else if (all(x7s4$sak_beidz[c(2,4,6,7)] == "1")) {
            if (all(sapply(c(1,2,3,4,6), function(i) diff(x7s4$NDZ_sanemsanas_datums[i:(i+1)]) != 0)) &&
                diff(x7s4$NDZ_sanemsanas_datums[5:6]) == 0) {
                if (x7s4$period[1] == "______" && x7s4$PS_code[1] == '___________' && x7s4$NM_code[1] == '______') {
                  x7s4_uzVieniniekiem <- rbind(x7s4_uzVieniniekiem, x7s4[c(1,7),])
                  x7s4_uzCetriniekiem <- rbind(x7s4_uzCetriniekiem, x7s4[c(2,3,4,5), ])
                } else {stop("processingSeven_s4 trūkst izstrādes koda.\n")}
            } else if (all(diff(x7s4$NDZ_sanemsanas_datums) != 0)) {
              if ((x7s4$period[1] == "______" && x7s4$PS_code[1] == '___________' && x7s4$NM_code[1] == '___________') ||
                  (x7s4$period[1] == "______" && x7s4$PS_code[1] == '______' && x7s4$NM_code[1] == '______')) {
                  x7s4_uzVieniniekiem <- rbind(x7s4_uzVieniniekiem, x7s4[c(1,7),])
                  x7s4_uzCetriniekiem <- rbind(x7s4_uzCetriniekiem, x7s4[c(2,3,4,5), ])
              } else {stop("processingSeven_s4 trūkst izstrādes koda.\n")}
            } else {stop("processingSeven_s4 trūkst izstrādes koda.\n")}
  } else if (all(x7s4$sak_beidz[c(2,3,6,7)] == "1")) {
              if (all(sapply(c(1,3,5), function(i) diff(x7s4$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
                  all(sapply(c(2,4,6), function(i) diff(x7s4$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
                if (x7s4$period[1] == "______" && x7s4$PS_code[1] == "______" && x7s4$NM_code[1] == "______") {
                  x7s4_uzSesiniekiem <- rbind(x7s4_uzSesiniekiem, x7s4[c(2,1,3,4,6,5), ])
                  x7s4_uzVieniniekiem <- rbind(x7s4_uzVieniniekiem, x7s4[7, ])
                } else {stop("processingSeven_s4 trūkst izstrādes koda.\n")}
              } else {stop("processingSeven_s4 trūkst izstrādes koda.\n")}
  } else if (all(x7s4$sak_beidz[c(1,2,4,6)] == "1")) {
    if (all(sapply(c(1,3,5), function(i) diff(x7s4$NDZ_sanemsanas_datums[i:(i+1)]) != 0)) &&
        all(sapply(c(2,4,6), function(i) diff(x7s4$NDZ_sanemsanas_datums[i:(i+1)]) == 0))) {
      if ((x7s4$period[1] == "______" && x7s4$PS_code[1] == "______" && x7s4$NM_code[1] == "______") ||
          (x7s4$period[1] == "______" && x7s4$PS_code[1] == "______" && x7s4$NM_code[1] == "______")) {
        x7s4 <- x7s4[c(1,3,2,5,4,7,6),]
        x7s4_uzSesiniekiem <- rbind(x7s4_uzSesiniekiem, x7s4[1:6, ])
        x7s4_uzVieniniekiem <- rbind(x7s4_uzVieniniekiem, x7s4[7, ])
        if (kods %in% c("40", "50", "53") && o == "7") {ZERO_minus(x7s4 %>% slice(1))}
      } else {stop("processingSeven_s4 trūkst izstrādes koda.\n")}
    } else {stop("processingSeven_s4 trūkst izstrādes koda.\n")}
  } else if (all(x7s4$sak_beidz[c(1,4,6,7)] == "1")) {
               if (all(diff(x7s4$NDZ_sanemsanas_datums) != 0)) {
                 if (x7s4$period[1] == "______" && x7s4$PS_code[1] == "______" && x7s4$NM_code[1] == "______") {
                   x7s4_uzSesiniekiem <- rbind(x7s4_uzSesiniekiem, x7s4[c(1,3,4,5), ])
                   x7s4_uzVieniniekiem <- rbind(x7s4_uzVieniniekiem, x7s4[7, ])
                   if (kods %in% c("40", "50", "53") && o == "7") {ZERO_minus(x7s4 %>% slice(1))}
                  } else {stop("processingSeven_s4 trūkst izstrādes koda.\n")}
                } else {stop("processingSeven_s4 trūkst izstrādes koda.\n")}
  #} else if (x7s4$sak_beidz[1] == x7s4$sak_beidz[2]) {
  #        if (diff(x7s4$NDZ_sanemsanas_datums[1:2]) != 0) {
  #          x7s4_uzSesiniekiem <- rbind(x7s4_uzSesiniekiem, x7s4[-1, ])
  #        } else {stop("processingSeven_s4 trūkst izstrādes koda.\n")}
  } else {stop("processingSeven_s4 trūkst izstrādes koda.\n")}
  
  rm(x7s4, o, kods)
  return(list(x7s4_uzVieniniekiem = x7s4_uzVieniniekiem,
              x7s4_uzDivniekiem   = x7s4_uzDivniekiem,
              x7s4_uzTrijniekiem  = x7s4_uzTrijniekiem,
              x7s4_uzCetriniekiem = x7s4_uzCetriniekiem, 
              x7s4_uzPieciniekiem = x7s4_uzPieciniekiem,
              x7s4_uzSesiniekiem  = x7s4_uzSesiniekiem))
}

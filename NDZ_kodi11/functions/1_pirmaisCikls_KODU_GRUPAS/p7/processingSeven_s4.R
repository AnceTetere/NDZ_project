processingSeven_s4 <- function(x7s4) {
  
  x7s4_uzVieniniekiem <- data.frame(); x7s4_uzDivniekiem <- data.frame(); x7s4_uzTrijniekiem <- data.frame(); x7s4_uzCetriniekiem <- data.frame(); x7s4_uzPieciniekiem <- data.frame(); x7s4_uzSesiniekiem <- data.frame()
  #x7s4 <- x7
  
  if (all(x7s4$sak_beidz[c(1,3,4,5)] == "1")) {
          if (all(sapply(c(1,3:6), function(i) diff(x7s4$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
            x7s4_uzTrijniekiem <- rbind(x7s4_uzTrijniekiem, x7s4[1:3, ])
            x7s4_uzCetriniekiem <- rbind(x7s4_uzCetriniekiem, x7s4[4:7, ])      
          } else if (all(sapply(c(1,2,3,5), function(i) diff(x7s4$NDZ_sanemsanas_datums[i:(i+1)]) != 0)) &&
                     all(sapply(c(4,6), function(i) diff(x7s4$NDZ_sanemsanas_datums[i:(i+1)]) == 0))) {
                      x7s4_uzVieniniekiem <- rbind(x7s4_uzVieniniekiem, x7s4[6, ])
                      x7s4_uzSesiniekiem <- rbind(x7s4_uzSesiniekiem, x7s4[c(1,2,3,5,4,7), ])
                      if (kods %in% c("40", "50", "53")) {ZERO_plus(x7s4 %>% slice(7)); ZERO_minus(x7s4 %>% slice(1))}
          } else {stop("processingSeven trūkst izstrādes koda.\n")}
  } else if (all(x7s4$sak_beidz[c(1,3,6,7)] == "1")) { 
               if (all(diff(x7s4$NDZ_sanemsanas_datums) != 0)) {
                 x7s4_uzCetriniekiem <- rbind(x7s4_uzCetriniekiem, x7s4[c(1:3,5), ])
                 x7s4_uzVieniniekiem <- rbind(x7s4_uzVieniniekiem, x7s4[7, ])
                 if (kods %in% c("40", "50", "53")) {ZERO_minus(x7s4 %>% slice(1))}
               } else if (all(sapply(c(1:4,6), function(i) diff(x7s4$NDZ_sanemsanas_datums[i:(i+1)]) != 0)) &&
                          diff(x7s4$NDZ_sanemsanas_datums[5:6]) == 0) {
                 x7s4_uzSesiniekiem <- rbind(x7s4_uzSesiniekiem, x7s4[c(1,2,3,4,6,5), ])
                 x7s4_uzVieniniekiem <- rbind(x7s4_uzVieniniekiem, x7s4[7, ])
                 if (kods %in% c("40", "50", "53")) {ZERO_minus(x7s4 %>% slice(1))}
               } else if (all(sapply(c(1,5), function(i) diff(x7s4$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
                          all(sapply(c(2,3,4,6), function(i) diff(x7s4$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
                 if (x7s4$period[1] == "________" && x7s4$PS_code[1] == "________" && x7s4$NM_code[1] == "________") {
                   x7s4_uzSesiniekiem <- rbind(x7s4_uzSesiniekiem, x7s4[c(1,2,3,4,6,5), ])
                   x7s4_uzVieniniekiem <- rbind(x7s4_uzVieniniekiem, x7s4[7, ])
                   if (kods %in% c("40", "50", "53")) {ZERO_minus(x7s4 %>% slice(1))}
                 } else {stop("processingSeven_s4 trūkst izstrādes koda.\n")}
               } else {stop("processingSeven trūkst izstrādes koda.\n")}
  } else if (all(x7s4$sak_beidz[c(2,3,5,7)] == "1")) {
            if (all(diff(x7s4$NDZ_sanemsanas_datums) != 0)) {
               x7s4_uzCetriniekiem <- rbind(x7s4_uzCetriniekiem, x7s4[3:6, ])
               x7s4_uzVieniniekiem <- rbind(x7s4_uzVieniniekiem, x7s4[c(1,7), ])
             } else if (all(sapply(2:6, function(i) diff(x7s4$NDZ_sanemsanas_datums[i:(i+1)]) != 0)) &&
                      diff(x7s4$NDZ_sanemsanas_datums[1:2]) == 0) {
                      x7s4_uzDivniekiem <- rbind(x7s4_uzDivniekiem, x7s4[c(2,1,3,4,5,6), ])
                      x7s4_uzVieniniekiem <- rbind(x7s4_uzVieniniekiem, x7s4[7, ])
             } else if (all(sapply(c(1,2,4,5,6), function(i) diff(x7s4$NDZ_sanemsanas_datums[i:(i+1)]) != 0)) &&
                        diff(x7s4$NDZ_sanemsanas_datums[3:4]) == 0) {
                        if (x7s4$PS_code[1] == '________' && x7s4$NM_code[1] == '________') {
                          x7s4_uzDivniekiem <- rbind(x7s4_uzDivniekiem, x7s4[c(3,4,5,6), ])
                          x7s4_uzVieniniekiem <- rbind(x7s4_uzVieniniekiem, x7s4[c(1,7), ])
                        } else {stop("processingSeven_s4 trūkst izstrādes koda.\n")}
             } else {stop("processingSeven_s4 trūkst izstrādes koda.\n")}
  } else if (all(x7s4$sak_beidz[c(1,3,5,7)] == "1")) {
          res <- processingSeven_s4_1212121(x7s4)
          if (exists("res")) {
            x7s4_uzVieniniekiem <- rbind(x7s4_uzVieniniekiem, res$x7s4_uzVieniniekiem)
            x7s4_uzSesiniekiem  <- rbind(x7s4_uzSesiniekiem, res$x7s4_uzSesiniekiem)
            rm(res)}    
  } else if (all(x7s4$sak_beidz[c(1,2,4,7)] == "1")) {
                if (all(sapply(c(2,4), function(i) diff(x7s4$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
                 all(sapply(c(1,3,5,6), function(i) diff(x7s4$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
                  if (x7s4$PS_code[1] == '________' && x7s4$NM_code[1] == '________') {
                    x7s4_uzDivniekiem <- rbind(x7s4_uzSesiniekiem, x7s4[c(2,3,4,6), ])
                    x7s4_uzVieniniekiem <- rbind(x7s4_uzVieniniekiem, x7s4[7, ])
                    if (kods %in% c("40", "50", "53")) {ZERO_minus(x7s4 %>% slice(1))}
                  } else {stop("processingSeven trūkst izstrādes koda.\n")}
              } else if (all(sapply(c(2,4,6), function(i) diff(x7s4$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
                         all(sapply(c(1,3,5), function(i) diff(x7s4$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
                         if (x7s4$PS_code[1] == '________' && x7s4$NM_code[1] == '________') {
                          x7s4_uzDivniekiem <- rbind(x7s4_uzSesiniekiem, x7s4[c(1,3,2,5,4,6), ])
                          x7s4_uzVieniniekiem <- rbind(x7s4_uzVieniniekiem, x7s4[7, ])
                          if (kods %in% c("40", "50", "53")) {ZERO_minus(x7s4 %>% slice(1))}
                        } else {stop("processingSeven trūkst izstrādes koda.\n")}
              } else {stop("processingSeven trūkst izstrādes koda.\n")}
  } else if (all(x7s4$sak_beidz[c(1,3,5,6)] == "1")) {
              if (all(diff(x7s4$NDZ_sanemsanas_datums) != 0)) {
                x7s4_uzSesiniekiem <- rbind(x7s4_uzSesiniekiem, x7s4[c(1,2,3,4,6,7), ])
                if (kods %in% c("40", "50", "53")) {ZERO_plus(x7s4 %>% slice(7)); ZERO_minus(x7s4 %>% slice(1))}
              } else {stop("processingSeven_s4 trūkst izstrādes koda.\n")}
  } else if (all(x7s4$sak_beidz[c(2,4,6,7)] == "1")) {
            if (all(sapply(c(1,2,3,4,6), function(i) diff(x7s4$NDZ_sanemsanas_datums[i:(i+1)]) != 0)) &&
                diff(x7s4$NDZ_sanemsanas_datums[5:6]) == 0) {
                if (x7s4$period[1] == "________" && x7s4$PS_code[1] == '________' && x7s4$NM_code[1] == '________') {
                  x7s4_uzVieniniekiem <- rbind(x7s4_uzVieniniekiem, x7s4[c(1,7),])
                  x7s4_uzCetriniekiem <- rbind(x7s4_uzCetriniekiem, x7s4[c(2,3,4,5), ])
                } else {stop("processingSeven_s4 trūkst izstrādes koda.\n")}
            } else if (all(diff(x7s4$NDZ_sanemsanas_datums) != 0)) {
              if ((x7s4$period[1] == "________" && x7s4$PS_code[1] == '________' && x7s4$NM_code[1] == '________') ||
                  (x7s4$period[1] == "________" && x7s4$PS_code[1] == '________' && x7s4$NM_code[1] == '________')) {
                  x7s4_uzVieniniekiem <- rbind(x7s4_uzVieniniekiem, x7s4[c(1,7),])
                  x7s4_uzCetriniekiem <- rbind(x7s4_uzCetriniekiem, x7s4[c(2,3,4,5), ])
              } else {stop("processingSeven_s4 trūkst izstrādes koda.\n")}
            } else {stop("processingSeven_s4 trūkst izstrādes koda.\n")}
  } else if (all(x7s4$sak_beidz[c(2,3,6,7)] == "1")) {
              if (all(sapply(c(1,3,5), function(i) diff(x7s4$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
                  all(sapply(c(2,4,6), function(i) diff(x7s4$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
                if (x7s4$period[1] == "________" && x7s4$PS_code[1] == "________" && x7s4$NM_code[1] == "________") {
                  x7s4_uzSesiniekiem <- rbind(x7s4_uzSesiniekiem, x7s4[c(2,1,3,4,6,5), ])
                  x7s4_uzVieniniekiem <- rbind(x7s4_uzVieniniekiem, x7s4[7, ])
                } else {stop("processingSeven_s4 trūkst izstrādes koda.\n")}
              } else {stop("processingSeven_s4 trūkst izstrādes koda.\n")}
  #} else if (x7s4$sak_beidz[1] == x7s4$sak_beidz[2]) {
  #        if (diff(x7s4$NDZ_sanemsanas_datums[1:2]) != 0) {
  #          x7s4_uzSesiniekiem <- rbind(x7s4_uzSesiniekiem, x7s4[-1, ])
  #        } else {stop("processingSeven_s4 trūkst izstrādes koda.\n")}
  } else {stop("processingSeven_s4 trūkst izstrādes koda.\n")}
  
  return(list(x7_uzVieniniekiem = x7s4_uzVieniniekiem,
              x7_uzDivniekiem   = x7s4_uzDivniekiem,
              x7_uzTrijniekiem  = x7s4_uzTrijniekiem,
              x7_uzCetriniekiem = x7s4_uzCetriniekiem, 
              x7_uzPieciniekiem = x7s4_uzPieciniekiem,
              x7_uzSesiniekiem  = x7s4_uzSesiniekiem))
}

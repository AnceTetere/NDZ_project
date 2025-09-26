processingSixes_s4 <- function(x6s4, o, kods) {
  #x6s4 <- x6
  x6s4_uzVieniniekiem <- data.frame(); x6s4_uzDivniekiem <- data.frame();
  x6s4_uzCetri <- data.frame(); x6s4_uzPieciniekiem <- data.frame()
  
  if (all(x6s4$sak_beidz[c(1,2,4,6)] == "1")) { 
           if (all(diff(x6s4$NDZ_sanemsanas_datums) != 0)) {
                x6s4_uzCetri <- x6s4[2:5, ]
                x6s4_uzVieniniekiem <- x6s4[6, ]
                if (kods %in% c("40", "50", "53") && o == "6") {ZERO_minus(x6s4 %>% slice(1))}
           } else if (diff(x6s4$NDZ_sanemsanas_datums[1:2]) == 0 && all(diff(x6s4$NDZ_sanemsanas_datums[2:6]) != 0)) {
                 if (kods == "40") {
                   if (x6s4$period[1] == '______' && x6s4$PS_code[1] ==  '______________' && x6s4$NM_code[1] ==  '______________') {
                        x6s4_uzCetri <- x6s4[2:5, ]; x6s4_uzVieniniekiem <- x6s4[6, ]
                     if (kods %in% c("40", "50", "53") && o == "6") {ZERO_minus(x6s4 %>% slice(1))}
                   } else {stop("processingSixes_s4 trūkst apstrādes koda. \n")}
                 } else {stop("processingSixes_s4 trūkst apstrādes koda. \n")}
            } else {stop("processingSixes_s4 trūkst apstrādes koda. \n")}
  } else if (all(x6s4$sak_beidz == c("1","1","2","1","1","2"))) {
            if (all(sapply(c(2,5), function(i) diff(x6s4$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
                all(sapply(c(1,3,4), function(i) diff(x6s4$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
                    if (x6s4$period[1] == '______' && x6s4$PS_code[1] ==  '______________' && x6s4$NM_code[1] ==  '______________') {
                        x6s4_uzCetri <- x6s4[c(1,3,5,6), ]
                        if (kods %in% c("40", "50", "53") && o == "6") {ZERO_minus(x6s4 %>% slice(1)); ZERO_plus(x6s4 %>% slice(6))}
                    } else {stop("processingSixes_s4 trūkst apstrādes koda. \n")}
            } else if (all(sapply(c(1,2,4,5), function(i) diff(x6s4$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
                       diff(x6s4$NDZ_sanemsanas_datums[3:4]) != 0) {
                        if (x6s4$period[1] == '______' && x6s4$PS_code[1] ==  '______________' && x6s4$NM_code[1] ==  '______________') {
                          x6s4_uzCetri <- x6s4[c(3,2,6,4), ]
                       } else {stop("processingSixes_s4 trūkst apstrādes koda. \n")}
            } else if (all(diff(x6s4$NDZ_sanemsanas_datums[1:5]) != 0) && diff(x6s4$NDZ_sanemsanas_datums[5:6]) == 0) {
                       if (x6s4$period[1] == '______' && x6s4$PS_code[1] ==  '______________' && x6s4$NM_code[1] ==  '______________') {
                           x6s4_uzCetri <- x6s4[c(2,3,5,6), ]
                           if (kods %in% c("40", "50", "53") && o == "6") {ZERO_minus(x6s4 %>% slice(2)); ZERO_plus(x6s4 %>% slice(6))}
              } else {stop("processingSixes_s4 trūkst apstrādes koda. \n")}
            } else {stop("processingSixes_s4 trūkst apstrādes koda. \n")}
  } else if (all(x6s4$sak_beidz == c("1","2","1","2","1","1"))) {
              if (all(sapply(c(1,2,4,5), function(i) diff(x6s4$NDZ_sanemsanas_datums[i:(i+1)]) != 0)) &&
                  diff(x6s4$NDZ_sanemsanas_datums[3:4]) == 0) {
                       x6s4_uzCetri <- x6s4[1:4, ]; x6s4_uzVieniniekiem <- x6s4[6, ]
                       if (kods %in% c("40", "50", "53") && o == "6") {ZERO_minus(x6s4 %>% slice(1))}
              } else if (all(diff(x6s4$NDZ_sanemsanas_datums) != 0)) {
                if (x6s4$period[1] == '______' && x6s4$PS_code[1] ==  '______________' && x6s4$NM_code[1] ==  '______________') {
                       x6s4_uzCetri <- x6s4[1:4, ]; x6s4_uzVieniniekiem <- x6s4[6, ]
                       if (kods %in% c("40", "50", "53") && o == "6") {ZERO_minus(x6s4 %>% slice(1))}
                } else {stop("processingSixes_s4 trūkst apstrādes koda. \n")}
              } else if (diff(x6s4$NDZ_sanemsanas_datums[1:2]) == 0 && all(diff(x6s4$NDZ_sanemsanas_datums[2:6]) != 0)) {
                if (x6s4$period[1] == '______' && x6s4$PS_code[1] ==  '______________' && x6s4$NM_code[1] ==  '______________') {
                  x6s4_uzCetri <- x6s4[1:4, ]; x6s4_uzVieniniekiem <- x6s4[6, ]
                  if (kods %in% c("40", "50", "53") && o == "6") {ZERO_minus(x6s4 %>% slice(1))}
                } else {stop("processingSixes_s4 trūkst apstrādes koda. \n")}
              } else {stop("processingSixes_s4 trūkst apstrādes koda. \n")}
  } else if (all(x6s4$sak_beidz[c(1,3,4,6)] == "1")) {
           if (all(sapply(c(1,4), function(i) diff(x6s4$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
               all(sapply(c(2,3,5), function(i) diff(x6s4$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
              if (x6s4$period[1] == '______' && x6s4$PS_code[1] ==  '______________' && x6s4$NM_code[1] ==  '______________') {
                   x6s4 <- x6s4[c(2,4,5,6),]
                   x6s4_uzVieniniekiem <- x6s4[c(1,4),]; x6s4_uzDivniekiem <- x6s4[2:3,]
                
            } else if ((x6s4$period[1] == '______' && x6s4$PS_code[1] ==  '______________' && x6s4$NM_code[1] ==  '______________') ||
                       (x6s4$period[1] == '______' && x6s4$PS_code[1] ==  '______________' && x6s4$NM_code[1] ==  '______________')) {
                       x6s4_uzVieniniekiem <- x6s4[6,]; x6s4_uzCetri <- x6s4[c(1,2,4,5), ]
                       if (kods %in% c("40", "50", "53") && o == "6") {ZERO_minus(x6s4 %>% slice(1))}
                       } else {stop("processingSixes_s4: trūkst apstrādes koda. \n")}
          } else if (all(diff(x6s4$NDZ_sanemsanas_datums) != 0)) {
                     if ((x6s4$PS_code[1] ==  '______________' && x6s4$NM_code[1] ==  '______________') ||
                         (x6s4$period[1] == '______' && x6s4$PS_code[1] ==  '______________' && x6s4$NM_code[1] ==  '______________') ||
                         (x6s4$period[1] == '______' && x6s4$PS_code[1] ==  '______________' && x6s4$NM_code[1] ==  '______________') ||
                         (x6s4$period[1] == '______' && x6s4$PS_code[1] ==  '______________' && x6s4$NM_code[1] ==  '______________') ||
                         (x6s4$period[1] == '______' && x6s4$PS_code[1] ==  '______________' && x6s4$NM_code[1] ==  '______________') ||
                         (x6s4$period[1] == '______' && x6s4$PS_code[1] ==  '______________' && x6s4$NM_code[1] ==  '______________')) {
                            x6s4_uzCetri <- x6s4[c(1,2,4,5), ]; x6s4_uzVieniniekiem <- x6s4[6, ]
                            if (kods %in% c("40", "50", "53") && o == "6") {ZERO_minus(x6s4 %>% slice(1))}
                     } else {stop("processingSixes_s4: trūkst apstrādes koda. \n")}
        } else {stop("processingSixes_s4: trūkst apstrādes koda. \n")}
  } else if (all(x6s4$sak_beidz == c("1","1","2","2","1","1"))) {
             if (kods == "40") {
               if (all(sapply(c(3,5), function(i) diff(x6s4$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
                   all(sapply(c(1,2,4), function(i) diff(x6s4$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
                         if (x6s4$period[1] == '______' && x6s4$PS_code[1] ==  '______________' && x6s4$NM_code[1] ==  '______________') {
                             x6s4_uzVieniniekiem <- x6s4[6,]; x6s4_uzDivniekiem <- x6s4[c(2,4), ]
                             if (kods %in% c("40", "50", "53") && o == "6") {ZERO_minus(x6s4 %>% slice(1))}
                         } else {stop("processingSixes_s4 trūkst apstrādes koda. \n")}
               } else if (all(sapply(seq(1,6,by=2), function(i) diff(x6s4$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
                          all(sapply(c(2,4), function(i) diff(x6s4$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
                          if (x6s4$period[1] == '______' && x6s4$PS_code[1] ==  '______________' && x6s4$NM_code[1] ==  '______________') {
                              x6s4_uzVieniniekiem <- x6s4[6,]; x6s4_uzDivniekiem <- x6s4[c(2,4), ]
                              if (kods %in% c("40", "50", "53") && o == "6") {ZERO_minus(x6s4 %>% slice(1))}
                         } else {stop("processingSixes_s4 trūkst apstrādes koda. \n")}
               } else if (diff(x6s4$NDZ_sanemsanas_datums[3:4]) == 0 && all(sapply(c(1,2,4,5), function(i) diff(x6s4$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
                          if (x6s4$period[1] == '______' && x6s4$PS_code[1] ==  '______________' && x6s4$NM_code[1] ==  '______________') {
                              x6s4_uzVieniniekiem <- x6s4[6,]; x6s4_uzDivniekiem <- x6s4[c(2,4), ]
                              if (kods %in% c("40", "50", "53") && o == "6") {ZERO_minus(x6s4 %>% slice(1))}
                          } else {stop("processingSixes_s4 trūkst apstrādes koda. \n")}
               } else {stop("processingSixes_s4: trūkst apstrādes koda. \n")}
            } else {stop("processingSixes_s4: trūkst apstrādes koda. \n")}
    } else {stop("processingSixes_s4: trūkst apstrādes koda. \n")}
  
    return(list(x6_uzVieniniekiem = x6s4_uzVieniniekiem, 
                x6_uzDivniekiem = x6s4_uzDivniekiem,
                x6_uzCetri = x6s4_uzCetri,
                x6_uzPieciniekiem = x6s4_uzPieciniekiem))
  } 

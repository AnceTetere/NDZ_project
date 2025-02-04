processingSixes_s4 <- function(x6s4) {
  #x6s4 <- x6
  x6s4_uzVieniniekiem <- data.frame(); x6s4_uzDivniekiem <- data.frame();
  x6s4_uzCetri <- data.frame(); x6s4_uzPieciniekiem <- data.frame()
  
  if(all(x6s4$sak_beidz == c("1","1","2","1","2","1")) && 
     all(diff(x6s4$NDZ_sanemsanas_datums) != 0)) {
    x6s4_uzCetri <- rbind(x6s4_uzCetri, x6s4[2:5, ])
    x6s4_uzVieniniekiem <- rbind(x6s4_uzVieniniekiem, x6s4[6, ])
  } else if (all(x6s4$sak_beidz == c("1","1","2","1","1","2"))) {
            if (all(sapply(c(2,5), function(i) diff(x6s4$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
                all(sapply(c(1,3,4), function(i) diff(x6s4$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
              if (x6s4$period[1] == '_________' && x6s4$PS_code[1] == '___________' && x6s4$NM_code[1] == "___________") {
                x6s4_uzCetri <- rbind(x6s4_uzCetri, x6s4[c(1,3,5,6), ])
              } else {stop("processingSixes_s4 trūkst apstrādes koda. \n")}
            } else if (all(sapply(c(1,2,4,5), function(i) diff(x6s4$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
                       diff(x6s4$NDZ_sanemsanas_datums[3:4]) != 0) {
                        if (x6s4$period[1] == '_________' && x6s4$PS_code[1] == '___________' && x6s4$NM_code[1] == "_________") {
                          x6s4_uzCetri <- rbind(x6s4_uzCetri, x6s4[c(3,2,6,4), ])
              } else {stop("processingSixes_s4 trūkst apstrādes koda. \n")}
            } else {stop("processingSixes_s4 trūkst apstrādes koda. \n")}
  } else if (all(x6s4$sak_beidz == c("1","2","1","2","1","1"))) {
          if (all(sapply(c(1,2,4,5), function(i) diff(x6s4$NDZ_sanemsanas_datums[i:(i+1)]) != 0)) &&
            diff(x6s4$NDZ_sanemsanas_datums[3:4]) == 0) {
            x6s4_uzCetri <- rbind(x6s4_uzCetri, x6s4[1:4, ])
            x6s4_uzVieniniekiem <- rbind(x6s4_uzVieniniekiem, x6s4[6, ])
          } else if (all(diff(x6s4$NDZ_sanemsanas_datums) != 0)) {
            if (x6s4$period[1] == "_________" && x6s4$PS_code[1] == '_________' && x6s4$NM_code[1] == '_________') {
              x6s4_uzCetri <- rbind(x6s4_uzCetri, x6s4[1:4, ])
              x6s4_uzVieniniekiem <- rbind(x6s4_uzVieniniekiem, x6s4[6, ])
            } else {stop("processingSixes_s4 trūkst apstrādes koda. \n")}
          } else {stop("processingSixes_s4 trūkst apstrādes koda. \n")}
  } else if (all(x6s4$sak_beidz == c("1", "2", "1", "1", "2", "1"))) {
         if (all(sapply(c(1,4), function(i) diff(x6s4$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
             all(sapply(c(2,3,5), function(i) diff(x6s4$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
            if ((x6s4$PS_code[1] == '_________' && x6s4$NM_code[1] == '_________') ||
                (x6s4$period[1] == '_________' && x6s4$PS_code[1] == '_________' && x6s4$NM_code[1] == '_________')) {
                x6s4_uzCetri <- rbind(x6s4_uzCetri, x6s4[c(2,3,5,6), ])
            } else {stop("processingSixes_s4: trūkst apstrādes koda sešinieku apakštabulai!\n Rinda ", r, " līdz ", r+5, "\n")}
          } else if (all(diff(x6s4$NDZ_sanemsanas_datums) != 0)) {
            if ((x6s4$PS_code[1] == '_________' && x6s4$NM_code[1] == '_________') ||
              (x6s4$period[1] == '_________' && x6s4$PS_code[1] == '_________' && x6s4$NM_code[1] == '_________') ||
              (x6s4$period[1] == '_________' && x6s4$PS_code[1] == '_________' && x6s4$NM_code[1] == '_________') ||
              (x6s4$period[1] == '_________' && x6s4$PS_code[1] == '_________' && x6s4$NM_code[1] == '_________')) {
            x6s4_uzCetri <- rbind(x6s4_uzCetri, x6s4[c(1,2,4,5), ])
            x6s4_uzVieniniekiem <- rbind(x6s4_uzVieniniekiem, x6s4[6, ])
          } else {stop("processingSixes_s4: trūkst apstrādes koda. \n")}
        } else {stop("processingSixes_s4: trūkst apstrādes koda. \n")}
  } else {stop("processingSixes_s4: trūkst apstrādes koda. \n")}
  
    return(list(x6_uzVieniniekiem = x6s4_uzVieniniekiem, 
                x6_uzDivniekiem = x6s4_uzDivniekiem,
                x6_uzCetri = x6s4_uzCetri,
                x6_uzPieciniekiem = x6s4_uzPieciniekiem))
  }

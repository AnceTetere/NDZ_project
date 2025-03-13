processingSeven_b4 <- function(x7b4) {

x7b4_uzVieniniekiem <- data.frame(); x7b4_uzDivniekiem <- data.frame(); x7b4_uzTrijniekiem <- data.frame()
x7b4_uzCetriniekiem <- data.frame(); x7b4_uzPieciniekiem <- data.frame(); x7b4_uzSesiniekiem <- data.frame()
#x7b4 <- x7

  if ((all(x7b4$sak_beidz[1:2] == c("2", "1")) && diff(x7b4$NDZ_sanemsanas_datums[1:2]) != 0) || 
      (x7b4$sak_beidz[1] == x7b4$sak_beidz[2] && diff(x7b4$NDZ_sanemsanas_datums[2:3]) == 0)) {
    x7b4_uzVieniniekiem <- rbind(x7b4_uzVieniniekiem, x7b4[1, ])
    x7b4_uzSesiniekiem <- rbind(x7b4_uzSesiniekiem, x7b4[-1, ])
  } else if (all(x7b4$sak_beidz == c("2","1","2","1","2","2","1")) && 
             diff(x7b4$NDZ_sanemsanas_datums[1:2]) == 0 && 
             x7b4$PS_code[1] == 'PK5F7171903' && x7b4$NM_code[1] == '41503007355') {
    #Pagaidām sabloķēju, jo neesmu droša, ka šis vispārinās.
    x7b4_uzVieniniekiem <- rbind(x7b4_uzVieniniekiem, x7b4[1, ])
    x7b4_uzSesiniekiem <- rbind(x7b4_uzSesiniekiem, x7b4[-1, ])
  } else if(all(x7b4$sak_beidz == c("2", "2", "1", "2", "1", "2", "1")) && all(diff(x7b4$NDZ_sanemsanas_datums) != 0)) {
    x7b4_uzVieniniekiem <- rbind(x7b4_uzVieniniekiem, x7b4[2, ])
    x7b4_uzPieciniekiem <- rbind(x7b4_uzCetriniekiem, x7b4[3:7, ])
  } else if(all(x7b4$sak_beidz == c("1", "2", "2", "2", "1", "1", "2")) && 
            diff(x7b4$NDZ_sanemsanas_datums[4:5]) == 0 && 
            x7b4$zinkod[3] == "26") {
    x7b4_uzDivniekiem <- rbind(x7b4_uzDivniekiem, x7b4[-2, ])
    if (kods %in% c("40", "50", "53") && o == "7") {ZERO_minus(x7b4 %>% slice(1)); ZERO_plus(x7b4 %>% slice(7))}
  } else if(all(x7b4$sak_beidz == c("2", "2", "1", "1", "2", "2", "1")) && 
            all(sapply(seq(1, 6, by = 2), function(i) diff(x7b4$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
            all(sapply(c(2,4,6), function(i) diff(x7b4$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
    x7b4_uzDivniekiem <- rbind(x7b4_uzDivniekiem, x7b4[4:5, ])
    x7b4_uzVieniniekiem <- rbind(x7b4_uzVieniniekiem, x7b4[c(1,7), ])
  } else if(all(x7b4$sak_beidz == c("1", "2", "2", "1", "2", "1", "2"))) {
            if (all(diff(x7b4$NDZ_sanemsanas_datums) != 0)) {
              if(x7b4$period[1] == "_____" && x7b4$PS_code[1] == "_____" && x7b4$NM_code[1] == '_____') {
                x7b4_uzDivniekiem <- rbind(x7b4_uzDivniekiem, x7b4[c(1,3,4,5,6,7), ])
                if (kods %in% c("40", "50", "53") && o == "7") {ZERO_minus(x7b4 %>% slice(1)); ZERO_plus(x7b4 %>% slice(7))}
              } else {stop("processingSeven_b4 trūkst izstrādes koda.\n")}
            } else {stop("processingSeven_b4 trūkst izstrādes koda.\n")}
  } else if (all(x7b4$sak_beidz[1:4] == c("1", "2", "1", "2"))) {
    if (all(diff(x7b4$NDZ_sanemsanas_datums[1:4]) != 0)) {
      if ((x7b4$period[1] == "_____" && x7b4$PS_code[1] == "_____" && x7b4$NM_code[1] == '_____') ||
          (x7b4$period[1] == "_____" && x7b4$PS_code[1] == "_____" && x7b4$NM_code[1] == '_____')) {
        x7b4_uzDivniekiem <- rbind(x7b4_uzDivniekiem, x7b4[1:2, ])
        x7b4_uzPieciniekiem <- rbind(x7b4_uzPieciniekiem, x7b4[3:7,])
        if (kods %in% c("40", "50", "53") && o == "7") {ZERO_minus(x7b4 %>% slice(1))}
      } else {stop("processingSeven_b4 trūkst izstrādes koda.\n")}
    } else {stop("processingSeven_b4 trūkst izstrādes koda.\n")}
  } else {stop("processingSeven_b4 trūkst izstrādes koda.\n")}
    
    return(list(x7_uzVieniniekiem = x7b4_uzVieniniekiem,
                x7_uzDivniekiem   = x7b4_uzDivniekiem,
                x7_uzTrijniekiem  = x7b4_uzTrijniekiem,
                x7_uzCetriniekiem = x7b4_uzCetriniekiem, 
                x7_uzPieciniekiem = x7b4_uzPieciniekiem,
                x7_uzSesiniekiem  = x7b4_uzSesiniekiem))
  }

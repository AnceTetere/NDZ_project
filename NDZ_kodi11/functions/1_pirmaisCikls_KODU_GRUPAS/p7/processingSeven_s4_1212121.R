processingSeven_s4_1212121 <- function(a, o, kods) {
  #a <- x7s4
  
  if (all(diff(a$NDZ_sanemsanas_datums) != 0)) {
          a6 <- a[1:6, ]; a1 <- a[7, ]
          if (kods %in% c("40", "50", "53") && o == "7") {ZERO_minus(a %>% slice(1))}
  } else if (all(sapply(c(1,3), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
             all(sapply(c(2,4,5,6), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
          a6 <- a[1:6, ]; a1 <- a[7, ]
          if (kods %in% c("40", "50", "53") && o == "7") {ZERO_minus(a %>% slice(1))}
  } else if (diff(a$NDZ_sanemsanas_datums[3:4]) == 0 &&
             all(sapply(c(1,2,4,5,6), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
          a6 <- a[1:6, ]; a1 <- a[7, ]
          if (kods %in% c("40", "50", "53") && o == "7") {ZERO_minus(a %>% slice(1))}
  } else if (all(sapply(c(3,5), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
             all(sapply(c(1,2,4,6), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
          a6 <- a[1:6, ]; a1 <- a[7, ]
          if (kods %in% c("40", "50", "53") && o == "7") {ZERO_minus(a %>% slice(1))}
  } else if (all(sapply(c(2,4,6), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
             all(sapply(c(1,3,5), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
                if (a$period[1] == "______" && a$PS_code == "___________" && a$NM_code == "___________") {
                a6 <- a[c(3,2,5,4,7,6), ]
               } else if (a$period[1] == "______" && a$NM_code == "___________") {
                            a6 <- a[1:6, ]; a1 <- a[7, ]
                            if (kods %in% c("40", "50", "53") && o == "7") {ZERO_minus(a %>% slice(1))}
              } else {stop("processingSeven_s4_1212121 trūkst izstrādes koda.\n")}
  } else if (all(sapply(seq(1,6,by=2), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
             all(sapply(seq(2,7,by=2), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
             if ((a$period[1] == "______" && a$PS_code == "___________" && a$NM_code == "___________") ||
                 (a$period[1] == "______" && a$PS_code == "___________" && a$NM_code == "___________") ||
                 (a$period[1] == "______" && a$PS_code == "___________" && a$NM_code == "___________")) {
               a1 <- a[7, ]; a6 <- a[1:6, ]
               if (kods %in% c("40", "50", "53") && o == "7") {ZERO_minus(a %>% slice(1))}
             } else {stop("processingSeven_s4_1212121 trūkst izstrādes koda.\n")}
  } else if (diff(a$NDZ_sanemsanas_datums[5:6]) == 0 &&
             all(sapply(c(1:4,6), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
            if ((a$period[1] == "______" && a$PS_code == "___________" && a$NM_code == "___________") ||
                (a$period[1] == "______" && a$PS_code == "___________" && a$NM_code == "___________") ||
                (a$period[1] == "______" && a$PS_code == "___________" && a$NM_code == "___________") ||
                (a$period[1] == "______" && a$PS_code == "___________" && a$NM_code == "___________") ||
                (a$period[1] == "______" && a$PS_code == "___________" && a$NM_code == "___________")) {
                  a1 <- a[7, ]; a6 <- a[1:6, ]
                  if (kods %in% c("40", "50", "53") && o == "7") {ZERO_minus(a %>% slice(1))}
            } else {stop("processingSeven_s4_1212121 trūkst izstrādes koda.\n")}
  } else if (diff(a$NDZ_sanemsanas_datums[1:2]) == 0 && all(diff(a$NDZ_sanemsanas_datums[2:7]) != 0)) {
              if ((a$period[1] == "______" && a$PS_code == "___________" && a$NM_code == "___________") ||
                  (a$period[1] == "______" && a$PS_code == "___________" && a$NM_code == "___________") ||
                  (a$period[1] == "______" && a$PS_code == "___________" && a$NM_code == "___________") ||
                  (a$period[1] == "______" && a$PS_code == "___________" && a$NM_code == "___________")) {
                    a1 <- a[7, ]; a6 <- a[1:6, ]
                    if (kods %in% c("40", "50", "53") && o == "7") {ZERO_minus(a %>% slice(1))}
    } else {stop("processingSeven_s4_1212121 trūkst izstrādes koda.\n")}
  } else {stop("processingSeven_s4_1212121 trūkst izstrādes koda.\n")}

  rm(a, o, kods)
  return(list(x7s4_uzVieniniekiem = a1,
              x7s4_uzDivniekiem = a2,
              x7s4_uzTrijniekiem = a3,
              x7s4_uzCetriniekiem = a4, 
              x7s4_uzPieciniekiem = a5,
              x7s4_uzSesiniekiem = a6))
}

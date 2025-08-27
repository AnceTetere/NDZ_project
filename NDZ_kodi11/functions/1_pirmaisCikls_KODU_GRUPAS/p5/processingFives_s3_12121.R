processingFives_s3_12121 <- function(x5s3s, o, kods) {
  x5s3s <- x5s3s %>% arrange(PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
  #x5s3s <- x5s3
  x5s3s_uzVieniniekiem <- data.frame(); x5s3s_uzDivniekiem <- data.frame(); x5s3s_uzCetriniekiem <- data.frame()
  
if (diff(x5s3s$NDZ_sanemsanas_datums[3:4]) == 0 && all(sapply(c(1,2,4), function(i) diff(x5s3s$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
              #IZTURĒJIS 10-nieku PĀRBAUDI
               x5s3s_uzVieniniekiem <- x5s3s[5,]; x5s3s_uzDivniekiem <- x5s3s[1:4,]
               if (kods %in% c("40", "50", "53") && o == "5") {ZERO_minus(x5s3s %>% slice(1))}
} else if (diff(x5s3s$NDZ_sanemsanas_datums[1:2]) == 0 && all(diff(x5s3s$NDZ_sanemsanas_datums[2:5]) != 0)) {
                #IZTURĒJIS 10-nieku PĀRBAUDI
                 x5s3s_uzVieniniekiem <- x5s3s[5,]; x5s3s_uzDivniekiem <- x5s3s[1:4,]
                 if (kods %in% c("40", "50", "53") && o == "5") {ZERO_minus(x5s3s %>% slice(1))}
} else if (all(sapply(c(1,3), function(i) diff(x5s3s$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
           all(sapply(c(2,4), function(i) diff(x5s3s$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
              if ((x5s3s$period[1] == '______' && x5s3s$PS_code[1] ==  '______________' && x5s3s$NM_code[1] ==  '______________') ||
                  (x5s3s$period[1] == '______' && x5s3s$PS_code[1] ==  '______________' && x5s3s$NM_code[1] ==  '______________') ||
                  (x5s3s$period[1] == '______' && x5s3s$PS_code[1] ==  '______________' && x5s3s$NM_code[1] ==  '______________') ||
                  (x5s3s$period[1] == '______' && x5s3s$PS_code[1] ==  '______________' && x5s3s$NM_code[1] ==  '______________') ||
                  (x5s3s$period[1] == '______' && x5s3s$PS_code[1] ==  '______________' && x5s3s$NM_code[1] ==  '______________') ||
                  (x5s3s$period[1] == '______' && x5s3s$PS_code[1] ==  '______________' && x5s3s$NM_code[1] ==  '______________')) {
                    x5s3s_uzVieniniekiem <- x5s3s[5,]; x5s3s_uzDivniekiem <- x5s3s[1:4,]
                    if (kods %in% c("40", "50", "53") && o == "5") {ZERO_minus(x5s3s %>% slice(1))}
              } else {stop("processingFives_s3_12121: Iztrūkst apstrādes koda!\n")}  
} else if (all(diff(x5s3s$NDZ_sanemsanas_datums) != 0)) {
          #Izturējis 10-nieku pārbaudi
          x5s3s_uzVieniniekiem <- x5s3s[5,]; x5s3s_uzDivniekiem <- x5s3s[1:4,]
              
#} else if (diff(x5s3s$NDZ_sanemsanas_datums[2:3]) == 0 && all(sapply(c(1,3,4), function(i) diff(x5s3s$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
#              x5s3s_uzVieniniekiem <- x5s3s[5,]; x5s3s_uzDivniekiem <- x5s3s[1:4,]
#} else if (all(sapply(c(1,4), function(i) diff(x5s3s$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) && 
#           all(diff(x5s3s$NDZ_sanemsanas_datums[2:4]) != 0)) {
#              x5s3s_uzVieniniekiem <- x5s3s[5,]; x5s3s_uzDivniekiem <- x5s3s[1:4,]
#              if (kods %in% c("40", "50", "53") && o == "5") {ZERO_minus(x5s3s %>% slice(1))}
} else if (diff(x5s3s$NDZ_sanemsanas_datums[4:5]) == 0 && all(diff(x5s3s$NDZ_sanemsanas_datums[1:4]) != 0)) {
              #JO PIRMOREIZ
              if (x5s3s$period[1] == '______' && x5s3s$PS_code[1] ==  '______________' && x5s3s$NM_code[1] ==  '______________') {
                x5s3s_uzVieniniekiem <- x5s3s[5,]; x5s3s_uzDivniekiem <- x5s3s[1:4,]
                if (kods %in% c("40", "50", "53") && o == "5") {ZERO_minus(x5s3s %>% slice(1))}
              } else {stop("processingFives_s3_12121: Iztrūkst apstrādes koda!\n")}
} else if (all(sapply(c(2,4), function(i) diff(x5s3s$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) && 
           all(sapply(c(1,3), function(i) diff(x5s3s$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
               #JO PIRMOREIZ
               if (x5s3s$period[1] == '______' && x5s3s$PS_code[1] ==  '______________' && x5s3s$NM_code[1] ==  '______________') {
                 x5s3s_uzCetriniekiem <- x5s3s[c(3,2,5,4),]
                 if (kods %in% c("40", "50", "53") && o == "5") {ZERO_minus(x5s3s %>% slice(3))}
               } else {stop("processingFives_s3_12121: Iztrūkst apstrādes koda!\n")}
} else {stop("processingFives_s3_12121: Iztrūkst apstrādes koda!\n")}
  
rm(x5s3s)
return(list(x5s3_uzVieniniekiem = x5s3s_uzVieniniekiem, 
            x5s3_uzDivniekiem = x5s3s_uzDivniekiem,
            x5s3_uzCetriniekiem = x5s3s_uzCetriniekiem)) 
}

processingFives_s2 <- function(x5s2s2) {  
  
  x5s2 <- x5s2[order(x5s2$PS_code, x5s2$DN_code, x5s2$NM_code, x5s2$NDZ_sanemsanas_datums), ]

x5s2_uzVieniniekiem <- data.frame()
x5s2_uzDivniekiem <- data.frame()
x5s2_uzCetriniekiem <- data.frame()

if(x5s2$end[1] == x5s2$end[2] && x5s2$end[2] == "2" && x5s2$start[3] == x5s2$start[4] && x5s2$start[3] == "1" && x5s2$end[5] == "2" && any(diff(x5s2$NDZ_sanemsanas_datums) != 0)) {
  x5s2_uzVieniniekiem <- rbind(x5s2_uzVieniniekiem, x5s2[2, ])
  x5s2_uzDivniekiem <- rbind(x5s2_uzDivniekiem, x5s2[4:5, ])
} else if (x5s2$start[1] == "1" && x5s2$end[2] == "2" && x5s2$end[3] == "2" && x5s2$start[4] == "1" && x5s2$end[5] == "2" && x5s2$NDZ_sanemsanas_datums[3] == x5s2$NDZ_sanemsanas_datums[4]) {
  x5s2_uzDivniekiem <- rbind(x5s2_uzDivniekiem, x5s2[-3, ])
} else if (x5s2$end[1] == "2" && x5s2$start[2] == "1" && x5s2$end[3] == "2" && x5s2$start[4] == "1" && x5s2$end[5] == "2" && 
           x5s2$NDZ_sanemsanas_datums[1] == x5s2$NDZ_sanemsanas_datums[2] && all(diff(x5s2$NDZ_sanemsanas_datums[2:5]) != 0) && x5s2$zinkod[3] == "26") {
  x5s2_uzVieniniekiem <- rbind(x5s2_uzVieniniekiem, x5s2[1, ])
  x5s2_uzCetriniekiem <- rbind(x5s2_uzCetriniekiem, x5s2[-1, ])
} else if (x5s2$end[1] == "2" && x5s2$start[2] == "1" && x5s2$end[3] == "2" && x5s2$start[4] == "1" && x5s2$end[5] == "2" && 
           x5s2$NDZ_sanemsanas_datums[3] == x5s2$NDZ_sanemsanas_datums[4] && all(diff(x5s2$NDZ_sanemsanas_datums[1:3]) != 0) && 
           all(diff(x5s2$NDZ_sanemsanas_datums[4:5]) != 0) && x5s2$zinkod[3] == "26") {
  x5s2_uzVieniniekiem <- rbind(x5s2_uzVieniniekiem, x5s2[1, ])
  x5s2_uzCetriniekiem <- rbind(x5s2_uzCetriniekiem, x5s2[-1, ])
} else if (all(x5s2$start[c(3,5)] == "1") &&  
           diff(x5s2$NDZ_sanemsanas_datums[1:2]) == diff(x5s2$NDZ_sanemsanas_datums[4:5]) &&
           all(diff(x5s2$NDZ_sanemsanas_datums[2:4]) != 0) && x5s2$PS_code[1] == '__________' && x5s2$NM_code[1] == '________________') {
  x5s2_uzVieniniekiem <- rbind(x5s2_uzVieniniekiem, x5s2[c(1,3), ])
} else if (all(x5s2$start[c(1,3)] == "1") &&  
           all(diff(x5s2$NDZ_sanemsanas_datums) != 0)) {
  x5s2_uzDivniekiem <- rbind(x5s2_uzDivniekiem, x5s2[c(1:3,5), ])
} else if (all(x5s2$start[c(2,4)] == "1") &&  
           all(diff(x5s2$NDZ_sanemsanas_datums[2:5]) != 0) &&
           all(diff(x5s2$NDZ_sanemsanas_datums[1:2]) == 0)) {
  x5s2_uzVieniniekiem <- rbind(x5s2_uzVieniniekiem, x5s2[1, ])
  x5s2_uzCetriniekiem <- rbind(x5s2_uzCetriniekiem, x5s2[2:5, ])
} else if (all(x5s2$start[c(2,5)] == "1") &&  
           all(diff(x5s2$NDZ_sanemsanas_datums) != 0)) {
  x5s2_uzVieniniekiem <- rbind(x5s2_uzVieniniekiem, x5s2[c(1,5), ])
  x5s2_uzDivniekiem<- rbind(x5s2_uzDivniekiem, x5s2[c(2,4), ])
} else if (all(x5s2$start[c(1,4)] == "1") &&  
           all(diff(x5s2$NDZ_sanemsanas_datums) != 0)) {
  x5s2_uzCetriniekiem<- rbind(x5s2_uzCetriniekiem, x5s2[c(1,3:5), ])
} else if (all(x5s2$start[4:5] == "1") &&  
           all(sapply(c(1,3,4), function(i) diff(x5s2$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
           diff(x5s2$NDZ_sanemsanas_datums[2:3]) != 0) {
  x5s2_uzVieniniekiem <- rbind(x5s2_uzVieniniekiem, x5s2[c(1,5), ])
} else {
  stop("processingFives_s2: Trūkst apstrādes koda.")
}

rm(x5s2)
return(list(x5_uzVieniniekiem = x5s2_uzVieniniekiem,
            x5_uzDivniekiem = x5s2_uzDivniekiem,
            x5_uzCetriniekiem = x5s2_uzCetriniekiem))
}

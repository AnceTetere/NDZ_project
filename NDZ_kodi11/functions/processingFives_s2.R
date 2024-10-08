processingFives_s2 <- function(x5s2) {  

  x5s2 <- x5s2[order(x5s2$PS_code, x5s2$DN_code, x5s2$NM_code, x5s2$NDZ_sanemsanas_datums), ]

  x5s2_uzVieniniekiem <- data.frame()
  x5s2_uzDivniekiem <- data.frame()
  x5s2_uzCetriniekiem <- data.frame()

if (all(diff(x5s2$NDZ_sanemsanas_datums) != 0)) {
  if(all(x5s2$sak_beidz[3:5] == "1")) {
    x5s2_uzVieniniekiem <- rbind(x5s2_uzVieniniekiem, x5s2[2, ])
    x5s2_uzDivniekiem <- rbind(x5s2_uzDivniekiem, x5s2[4:5, ])
  } else if (all(x5s2$sak_beidz[c(1,3)] == "1")) {
    x5s2_uzDivniekiem <- rbind(x5s2_uzDivniekiem, x5s2[c(1:3,5), ])
  } else if (all(x5s2$sak_beidz[c(2,5)] == "1")) {
    x5s2_uzVieniniekiem <- rbind(x5s2_uzVieniniekiem, x5s2[c(1,5), ])
    x5s2_uzDivniekiem<- rbind(x5s2_uzDivniekiem, x5s2[c(2,4), ])
  } else if (all(x5s2$sak_beidz[c(1,4)] == "1")) {
    x5s2_uzCetriniekiem<- rbind(x5s2_uzCetriniekiem, x5s2[c(1,3:5), ])
  } else {stop("processingFives_s2: Trūkst apstrādes koda.")}
} else if (diff(x5s2$NDZ_sanemsanas_datums[1:2] == 0) && all(diff(x5s2$NDZ_sanemsanas_datums[2:5]) != 0)) {
  if (all(x5s2$sak_beidz[1:5] == c("2", "1", "2", "1", "2")) && x5s2$zinkod[3] == "26") {
    x5s2_uzVieniniekiem <- rbind(x5s2_uzVieniniekiem, x5s2[1, ])
    x5s2_uzCetriniekiem <- rbind(x5s2_uzCetriniekiem, x5s2[-1, ])
  } else {stop("processingFives_s2: Trūkst apstrādes koda.")}
} else if (all(sapply(c(1,2,4), function(i) diff(x5s2$NDZ_sanemsanas_datums[i:(i+1)]) != 0)) && diff(x5s2$NDZ_sanemsanas_datums[3:4]) == 0) {
  if (all(x5s2$sak_beidz[1:5] == c("2", "1", "2", "1", "2")) && x5s2$zinkod[3] == "26") {
    x5s2_uzVieniniekiem <- rbind(x5s2_uzVieniniekiem, x5s2[1, ])
    x5s2_uzCetriniekiem <- rbind(x5s2_uzCetriniekiem, x5s2[-1, ])
  } else if (all(x5s2$sak_beidz[1:5] == c("1", "2", "2", "1", "2"))) {
    x5s2_uzDivniekiem <- rbind(x5s2_uzDivniekiem, x5s2[-3, ])
  } else {stop("processingFives_s2: Trūkst apstrādes koda.")}
} else if (all(diff(x5s2$NDZ_sanemsanas_datums[2:5]) != 0) && diff(x5s2$NDZ_sanemsanas_datums[1:2]) == 0) {
  if (all(x5s2$sak_beidz[c(1,4)] == "1")) {
    x5s2_uzCetriniekiem <- rbind(x5s2_uzCetriniekiem, x5s2[c(1,3:5), ])
  } else if (all(x5s2$sak_beidz[c(2,4)] == "1")) {
    x5s2_uzVieniniekiem <- rbind(x5s2_uzVieniniekiem, x5s2[1, ])
    x5s2_uzCetriniekiem <- rbind(x5s2_uzCetriniekiem, x5s2[2:5, ])
  } else {stop("processingFives_s2: Trūkst apstrādes koda.")}
} else if (all(sapply(c(1,3,4), function(i) diff(x5s2$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
           diff(x5s2$NDZ_sanemsanas_datums[2:3]) != 0) {
  if (all(x5s2$sak_beidz[4:5] == "1")) {
    x5s2_uzVieniniekiem <- rbind(x5s2_uzVieniniekiem, x5s2[c(1,5), ])
  } else {stop("processingFives_s2: Trūkst apstrādes koda.")}
} else if (all(x5s2$sak_beidz[c(3,5)] == "1") &&  
           diff(x5s2$NDZ_sanemsanas_datums[1:2]) == diff(x5s2$NDZ_sanemsanas_datums[4:5]) &&
           all(diff(x5s2$NDZ_sanemsanas_datums[2:4]) != 0) && x5s2$PS_code[1] == '__________' && x5s2$NM_code[1] == '__________') {
  x5s2_uzVieniniekiem <- rbind(x5s2_uzVieniniekiem, x5s2[c(1,3), ])
} else {
  stop("processingFives_s2: Trūkst apstrādes koda.")
}

rm(x5s2)
return(list(x5_uzVieniniekiem = x5s2_uzVieniniekiem,
            x5_uzDivniekiem = x5s2_uzDivniekiem,
            x5_uzCetriniekiem = x5s2_uzCetriniekiem))
}

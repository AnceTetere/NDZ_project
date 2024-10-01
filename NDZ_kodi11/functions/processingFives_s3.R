processingFives_s3 <- function(x5s3) {
  x5s3 <- x5s3[order(x5s3$PS_code, x5s3$DN_code, x5s3$NM_code, x5s3$NDZ_sanemsanas_datums), ]
  #x5s3 <- x5
  x5s3_uzVieniniekiem <- data.frame()
  x5s3_uzDivniekiem <- data.frame()
  x5s3_uzCetriniekiem <- data.frame()

if (all(diff(x5s3$NDZ_sanemsanas_datums[1:5]) != 0)) {
  if(all(x5s3$sak_beidz[c(2,4:5)] == "1")) {
    x5s3_uzVieniniekiem <- rbind(x5s3_uzVieniniekiem, x5s3[c(1, 5), ])
    x5s3_uzDivniekiem <- rbind(x5s3_uzDivniekiem, x5s3[2:3, ])
  } else if (all(x5s3$sak_beidz[c(2:3,5)] == "1")) {
    x5s3_uzVieniniekiem <- rbind(x5s3_uzVieniniekiem, x5s3[c(1, 5), ])
    x5s3_uzDivniekiem <- rbind(x5s3_uzDivniekiem, x5s3[3:4, ])
  } else if (all(x5s3$sak_beidz[2:4] == "1")) {
    x5s3_uzVieniniekiem <- rbind(x5s3_uzVieniniekiem, x5s3[1, ])
    x5s3_uzDivniekiem <- rbind(x5s3_uzDivniekiem, x5s3[4:5, ])
  } else {
    stop("processingFives_s3 iztrūkst apstrādes koda")
  }
} else if (all(x5s3$sak_beidz[c(1, 3, 5)] == "1") && any(diff(x5s3$NDZ_sanemsanas_datums[1:3]) != 0) && x5s3$NDZ_sanemsanas_datums[4] == x5s3$NDZ_sanemsanas_datums[5]) {
  x5s3_uzVieniniekiem <- rbind(x5s3_uzVieniniekiem, x5s3[5, ])
  x5s3_uzDivniekiem <- rbind(x5s3_uzDivniekiem, x5s3[1:4, ])
} else if (all(x5s3$sak_beidz[c(2:3,5)] == "1") && diff(x5s3$NDZ_sanemsanas_datums[1:2]) == 0) {
  x5s3_uzVieniniekiem <- rbind(x5s3_uzVieniniekiem, x5s3[5, ])
  x5s3_uzDivniekiem <- rbind(x5s3_uzDivniekiem, x5s3[1:4, ])
} else if (all(x5s3$sak_beidz[c(1,3,5)] == "1") && 
           all(diff(x5s3$NDZ_sanemsanas_datums[2:3]) == 0) && 
           all(diff(x5s3$NDZ_sanemsanas_datums[3:5]) != 0) && 
           all(diff(x5s3$NDZ_sanemsanas_datums[1:2]) != 0) && 
           x5s3$PS_code[1] == "________" && x5s3$NM_code[1] == "_____________") {
 x5s3_uzVieniniekiem <- rbind(x5s3_uzVieniniekiem, x5s3[5, ])
  x5s3_uzCetriniekiem <- rbind(x5s3_uzCetriniekiem, x5s3[-5, ])
} else if (all(x5s3$sak_beidz[1:3] == "1") && 
           all(diff(x5s3$NDZ_sanemsanas_datums[2:3]) == 0) && 
           all(diff(x5s3$NDZ_sanemsanas_datums[1:2]) != 0) && 
           all(diff(x5s3$NDZ_sanemsanas_datums[3:5]) != 0)) {
  x5s3_uzDivniekiem <- rbind(x5s3_uzDivniekiem, x5s3[c(3,5), ])
} else if (all(x5s3$sak_beidz[3:5] == "1") && 
           all(diff(x5s3$NDZ_sanemsanas_datums[1:3]) == 0) && 
           diff(x5s3$NDZ_sanemsanas_datums[4:5]) == 0 && diff(x5s3$NDZ_sanemsanas_datums[3:4]) != 0) {
  x5s3_uzVieniniekiem <- rbind(x5s3_uzVieniniekiem, x5s3[c(3,5), ])
} else if (all(x5s3$sak_beidz[3:5] == "1") && 
           all(diff(x5s3$NDZ_sanemsanas_datums[1:3]) == 0) && 
           all(diff(x5s3$NDZ_sanemsanas_datums[3:5]) != 0)) {
  x5s3_uzVieniniekiem <- rbind(x5s3_uzVieniniekiem, x5s3[c(1,5), ])
} else if (all(x5s3$sak_beidz[c(1,4,5)] == "1")) {
  if (all(sapply(seq(2,5, by=2), function(i) all(diff(x5s3$NDZ_sanemsanas_datums[i:(i+1)]) == 0))) && 
      all(sapply(c(1, 3), function(i) all(diff(x5s3$NDZ_sanemsanas_datums[i:(i+1)]) != 0)))) {
    x5s3_uzVieniniekiem <- rbind(x5s3_uzVieniniekiem, x5s3[5, ])
    x5s3_uzDivniekiem <- rbind(x5s3_uzDivniekiem, x5s3[c(1, 3), ])
  } else if (all(diff(x5s3$NDZ_sanemsanas_datums[1:3]) == 0) && 
             all(diff(x5s3$NDZ_sanemsanas_datums[3:5]) != 0)) {
    if (!("26" %in% x5s3$zinkod[x5s3$sak_beidz == "2"])) {
      x5s3_uzVieniniekiem <- rbind(x5s3_uzVieniniekiem, x5s3[5, ])
      x5s3_uzDivniekiem <- rbind(x5s3_uzDivniekiem, x5s3[c(1, 3), ])
    } else {stop("processingFives_s3: Iztrūkst apstrādes koda!\n")}
  } else {stop("processingFives_s3: Iztrūkst apstrādes koda!\n")}
} else if (all(x5s3$sak_beidz[c(1,3,4)] == "1") && all(diff(x5s3$NDZ_sanemsanas_datums) != 0)) {
  x5s3_uzCetriniekiem <- rbind(x5s3_uzCetriniekiem, x5s3[c(1, 2, 4, 5), ])
} else if (all(x5s3$sak_beidz[3:5] == "1") && all(diff(x5s3$NDZ_sanemsanas_datums[2:4]) != 0) &&
           all(sapply(c(1,4), function(i) diff(x5s3$NDZ_sanemsanas_datums[i:(i+1)]) == 0))) {
  x5s3_uzVieniniekiem <- rbind(x5s3_uzVieniniekiem, x5s3[c(1, 5), ])
} else {
  stop("processingFives_s3: Iztrūkst apstrādes koda!\n")}
  
  
  
rm(x5s3)
return(list(x5_uzVieniniekiem = x5s3_uzVieniniekiem, 
            x5_uzDivniekiem = x5s3_uzDivniekiem, 
            x5_uzCetriniekiem = x5s3_uzCetriniekiem))
}

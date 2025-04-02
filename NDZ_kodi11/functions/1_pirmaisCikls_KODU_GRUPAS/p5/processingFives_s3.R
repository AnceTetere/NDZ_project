processingFives_s3 <- function(x5s3, o, kods) {
  x5s3 <- x5s3 %>% arrange(PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
  #x5s3 <- x5
  x5s3_uzVieniniekiem <- data.frame(); x5s3_uzDivniekiem <- data.frame(); x5s3_uzCetriniekiem <- data.frame()

  result <- function(y) {
  x5s3_uzVieniniekiem <<- rbind(x5s3_uzVieniniekiem, y$x5s3_uzVieniniekiem)
  x5s3_uzDivniekiem <<- rbind(x5s3_uzDivniekiem, y$x5s3_uzDivniekiem)
  x5s3_uzCetriniekiem <<- rbind(x5s3_uzCetriniekiem, y$x5s3_uzCetriniekiem)
  rm(y)
  }
  
if (all(x5s3$sak_beidz[c(1,3,5)] == "1")) {
      result(processingFives_s3_12121(x5s3, o, kods))
      if (kods %in% c("40", "50", "53")) {ZERO_minus(x5s3 %>% slice(1))}
} else if (all(diff(x5s3$NDZ_sanemsanas_datums) != 0)) {
          result(processingFives_s3e2(x5s3, o, kods))
} else if (all(diff(x5s3$NDZ_sanemsanas_datums[1:3]) != 0) && diff(x5s3$NDZ_sanemsanas_datums[4:5]) == 0) {
          result(processingFives_s3e3(x5s3, o, kods))
} else if (diff(x5s3$NDZ_sanemsanas_datums[2:3]) == 0 && all(sapply(c(1,3,4), function(i) diff(x5s3$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
          result(processingFives_s3e4(x5s3, o, kods))
} else if (diff(x5s3$NDZ_sanemsanas_datums[2:3]) == 0 && all(sapply(c(1,3), function(i) x5s3$NDZ_sanemsanas_datums[i:(i+1)] != 0))) {
          result(processingFives_s3e5(x5s3, o, kods))        
} else if (all(sapply(c(1,2,4), function(i) x5s3$NDZ_sanemsanas_datums[i:(i+1)] == 0)) && diff(x5s3$NDZ_sanemsanas_datums[3:4]) != 0) {
            result(processingFives_s3e6(x5s3, o, kods))
} else if (all(diff(x5s3$NDZ_sanemsanas_datums[1:3]) == 0) && all(diff(x5s3$NDZ_sanemsanas_datums[3:5]) != 0)) {
            result(processingFives_s3e7(x5s3, o, kods))
} else if (all(sapply(c(2,4), function(i) diff(x5s3$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) && all(sapply(c(1,3), function(i) diff(x5s3$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
           result(processingFives_s3e8(x5s3, o, kods))
} else if (all(diff(x5s3$NDZ_sanemsanas_datums[1:3]) == 0) && all(diff(x5s3$NDZ_sanemsanas_datums[3:5]) != 0)) {
            result(processingFives_s3e9(x5s3, o, kods))        
} else if (all(diff(x5s3$NDZ_sanemsanas_datums[2:4]) != 0) && all(sapply(c(1,4), function(i) diff(x5s3$NDZ_sanemsanas_datums[i:(i+1)]) == 0))) {
            result(processingFives_s3e10(x5s3, o, kods))
} else if (diff(x5s3$NDZ_sanemsanas_datums[3:4]) == 0 && all(sapply(c(1,2,4), function(i) diff(x5s3$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
            result(processingFives_s3e11(x5s3, o, kods))
} else if (diff(x5s3$NDZ_sanemsanas_datums[1:2]) == 0 && all(sapply(2:4, function(i) diff(x5s3$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
            result(processingFives_s3e1(x5s3, o, kods))
} else if (all(sapply(c(1,3), function(i) diff(x5s3$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) && all(sapply(c(2,4), function(i) diff(x5s3$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
           result(processingFives_s3e12(x5s3, o, kods))
} else {stop("processingFives_s3: Iztrūkst apstrādes koda!\n")}
  
rm(x5s3, o, kods)
return(list(x5_uzVieniniekiem = x5s3_uzVieniniekiem, 
            x5_uzDivniekiem = x5s3_uzDivniekiem, 
            x5_uzCetriniekiem = x5s3_uzCetriniekiem))
}

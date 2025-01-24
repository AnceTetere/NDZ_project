processingFives_s2 <- function(x5s2, kods) {  
  #x5s2 <- x5 for testing
  x5s2 <- x5s2 %>% arrange(PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
  x5s2_uzVieniniekiem <- data.frame(); x5s2_uzDivniekiem <- data.frame(); x5s2_uzCetriniekiem <- data.frame()

  fncResult <- function(result) {
    if (exists("result")) {
      x5s2_uzVieniniekiem <<- rbind(x5s2_uzVieniniekiem, result$x5s2_uzVieniniekiem)
      x5_uzDivniekiem <<- rbind(x5s2_uzDivniekiem, result$x5s2_uzDivniekiem)
      x5_uzCetriniekiem <<- rbind(x5s2_uzCetriniekiem, result$x5s2_uzCetriniekiem)
    }
    rm(result)}
  
if (all(diff(x5s2$NDZ_sanemsanas_datums) != 0)) {
  fncResult(processingFives_s2e1(a, kods))
} else if (diff(x5s2$NDZ_sanemsanas_datums[1:2] == 0) && all(diff(x5s2$NDZ_sanemsanas_datums[2:5]) != 0)) {
  fncResult(processingFives_s2e2(a, kods))
} else if (all(sapply(c(1,2,4), function(i) diff(x5s2$NDZ_sanemsanas_datums[i:(i+1)]) != 0)) && diff(x5s2$NDZ_sanemsanas_datums[3:4]) == 0) {
  fncResult(processingFives_s2e3(a, kods))
} else if (all(diff(x5s2$NDZ_sanemsanas_datums[2:5]) != 0) && diff(x5s2$NDZ_sanemsanas_datums[1:2]) == 0) {
  fncResult(processingFives_s2e4(a, kods))
} else if (all(sapply(c(1,3,4), function(i) diff(x5s2$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
           diff(x5s2$NDZ_sanemsanas_datums[2:3]) != 0) {
  fncResult(processingFives_s2e5(a, kods))
} else if (all(sapply(c(1,3,4), function(i) diff(x5s2$NDZ_sanemsanas_datums[i:(i+1)]) != 0)) &&
           diff(x5s2$NDZ_sanemsanas_datums[2:3]) == 0) {
  fncResult(processingFives_s2e6(a, kods))
} else if (all(sapply(c(2,4), function(i) diff(x5s2$NDZ_sanemsanas_datums[i:(i+1)]) != 0)) &&
           all(sapply(c(1,3), function(i) diff(x5s2$NDZ_sanemsanas_datums[i:(i+1)]) == 0))) {
  fncResult(processingFives_s2e7(a, kods))
} else if (diff(x5s2$NDZ_sanemsanas_datums[4:5]) == 0 &&
           all(diff(x5s2$NDZ_sanemsanas_datums[1:4]) != 0)) {
  fncResult(processingFives_s2e8(a, kods))
} else if (all(sapply(c(2,4), function(i) diff(x5s2$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
           all(sapply(c(1,3), function(i) diff(x5s2$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
  fncResult(processingFives_s2e9(a, kods))
} else {stop("processingFives_s2: Trūkst apstrādes koda.")}

rm(x5s2, kods)
return(list(x5_uzVieniniekiem = x5s2_uzVieniniekiem,
            x5_uzDivniekiem = x5s2_uzDivniekiem,
            x5_uzCetriniekiem = x5s2_uzCetriniekiem))
}

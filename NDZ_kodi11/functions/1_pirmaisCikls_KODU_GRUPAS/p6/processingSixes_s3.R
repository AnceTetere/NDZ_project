processingSixes_s3 <- function(x6s3) {
  
  x6s3_uzVieniniekiem <- data.frame(); x6s3_uzDivniekiem <- data.frame();
  x6s3_uzCetri <- data.frame(); x6s3_uzPieciniekiem <- data.frame()
  
  if (all(x6s3$sak_beidz[c(1,3,5)] == "1")) {
    x6s3_uzDivniekiem <- rbind(x6s3_uzDivniekiem, x6s3)
  } else if(all(x6s3$sak_beidz == c("1","1","2","2","1","2")) && 
            all(diff(x6s3$NDZ_sanemsanas_datums[1:5]) != 0) && 
            diff(x6s3$NDZ_sanemsanas_datums[5:6]) == 0) {
    x6s3_uzDivniekiem <- rbind(x6s3_uzDivniekiem, x6s3[c(2,4,5,6), ])
  } else if(all(x6s3$sak_beidz == c("2", "2", "1", "1", "2", "1")) && 
            diff(x6s3$NDZ_sanemsanas_datums[2:3]) == 0 && all(diff(x6s3$NDZ_sanemsanas_datums[3:6]) != 0)) {
    x6s3_uzVieniniekiem <- rbind(x6s3_uzVieniniekiem, x6s3[c(1, 6), ])
    x6s3_uzDivniekiem <- rbind(x6s3_uzDivniekiem, x6s3[2:5, ])
  } else if (all(x6s3$sak_beidz == c("2","2","2","1","1","1")) && 
             diff(x6s3$NDZ_sanemsanas_datums[3:4]) != 0 && 
             all(sapply(seq(1,6, by=3), function(i) diff(x6s3$NDZ_sanemsanas_datums[i:(i+2)]) == 0))) {
    x6s3_uzDivniekiem <- rbind(x6s3_uzDivniekiem, x6s3[c(1,4), ])
  } else if (all(x6s3$sak_beidz == c("2", "1", "2", "1", "2", "1")) && 
             all(sapply(seq(1,4,by=2), function(i) diff(x6s3$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
             diff(x6s3$NDZ_sanemsanas_datums[5:6]) != 0 &&
             x6s3$PS_code[1] == "_________" && x6s3$NM_code[1] == "_________") {
    x6s3_uzVieniniekiem <- rbind(x6s3_uzVieniniekiem, x6s3[1, ])
    x6s3_uzPieciniekiem <- rbind(x6s3_uzPieciniekiem, x6s3[-1, ])
  } else if (all(x6s3$sak_beidz == c("2", "2", "1", "2", "1", "1")) && 
             all(sapply(seq(2,4, by=2), function(i) diff(x6s3$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
             all(sapply(seq(1,5, by=2), function(i) diff(x6s3$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
    x6s3_uzPieciniekiem <- rbind(x6s3_uzPieciniekiem, x6s3[-1, ])
  } else if (all(x6s3$sak_beidz == c("1","2","1","2","2","1")) && diff(x6s3$NDZ_sanemsanas_datums) != 0) {
    x6s3_uzCetri <- rbind(x6s3_uzCetri, x6s3[c(1,2,3,5), ])
    x6s3_uzVieniniekiem <- rbind(x6s3_uzVieniniekiem, x6s3[6,])
  } else if (all(x6s3$sak_beidz == c("1", "2", "1", "1", "2", "2")) && 
             all(sapply(c(1,2,3,5), function(i) diff(x6s3$NDZ_sanemsanas_datums[i:(i+1)]) != 0)) &&
             diff(x6s3$NDZ_sanemsanas_datums[4:5]) == 0) {
    x6s3_uzCetri <- rbind(x6s3_uzCetri, x6s3[c(1,2,3,5), ])
    x6s3_uzDivniekiem <- rbind(x6s3_uzDivniekiem, x6s3[c(4,6),])
  } else {stop("processingSixes_s3 trūkst apstrādes koda. \n")}
  
  
  return(list(x6_uzVieniniekiem = x6s3_uzVieniniekiem, 
              x6_uzDivniekiem = x6s3_uzDivniekiem,
              x6_uzCetri = x6s3_uzCetri,
              x6_uzPieciniekiem = x6s3_uzPieciniekiem))
}

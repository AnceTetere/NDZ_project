processingFives_s3_12121 <- function(x5s3s) {
  x5s3s %>% arrange(PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
  #x5s3s <- x5s3
  x5s3s_uzVieniniekiem <- data.frame()
  x5s3s_uzDivniekiem <- data.frame()
  
if (diff(x5s3$NDZ_sanemsanas_datums[3:4]) == 0 &&
      all(sapply(c(1,2,4), function(i) diff(x5s3$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
    if(x5s3$PS_code[1] == '___________' && x5s3$NM_code[1] == '___________') {
      x5s3s_uzVieniniekiem <- rbind(x5s3s_uzVieniniekiem, x5s3[5,])
      x5s3s_uzDivniekiem <- rbind(x5s3s_uzDivniekiem, x5s3[1:4,])
    } else {stop("processingFives_s3_12121: Iztrūkst apstrādes koda!\n")}
} else if (diff(x5s3$NDZ_sanemsanas_datums[1:2]) == 0 &&
           all(sapply(2:4, function(i) diff(x5s3$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
      x5s3s_uzVieniniekiem <- rbind(x5s3s_uzVieniniekiem, x5s3[5,])
      x5s3s_uzDivniekiem <- rbind(x5s3s_uzDivniekiem, x5s3[1:4,])
} else if (all(sapply(c(1,3), function(i) diff(x5s3$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
           all(sapply(c(2,4), function(i) diff(x5s3$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
      if((x5s3$PS_code[1] == '___________' && x5s3$NM_code[1] == '___________') ||
          (x5s3$PS_code[1] == '___________' && x5s3$NM_code[1] == '___________') ||
          (x5s3$PS_code[1] == '___________' && x5s3$NM_code[1] == '___________') ||
          (x5s3$PS_code[1] == '___________' && x5s3$NM_code[1] == '___________')) {
        x5s3s_uzVieniniekiem <- rbind(x5s3s_uzVieniniekiem, x5s3[5,])
        x5s3s_uzDivniekiem <- rbind(x5s3s_uzDivniekiem, x5s3[1:4,])
     } else {stop("processingFives_s3_12121: Iztrūkst apstrādes koda!\n")}  
} else {stop("processingFives_s3_12121: Iztrūkst apstrādes koda!\n")}
  
rm(x5s3s)
return(list(x5s3_uzVieniniekiem = x5s3s_uzVieniniekiem, 
              x5s3_uzCetriniekiem = x5s3s_uzCetriniekiem)) 
}

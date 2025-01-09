processingFives_s3 <- function(x5s3) {
  x5s3 <- x5s3 %>% arrange(PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
  #x5s3 <- x5
  x5s3_uzVieniniekiem <- data.frame(); x5s3_uzDivniekiem <- data.frame(); x5s3_uzCetriniekiem <- data.frame()

if (all(x5s3$sak_beidz == c("1", "2", "1", "2", "1"))) {
  result <- processingFives_s3_12121(x5s3)
  x5s3_uzVieniniekiem <- rbind(x5s3_uzVieniniekiem, result$x5s3_uzVieniniekiem)
  x5s3_uzDivniekiem <- rbind(x5s3_uzDivniekiem, result$x5s3_uzDivniekiem)
  rm(result)
} else if (all(diff(x5s3$NDZ_sanemsanas_datums) != 0)) {
          if(all(x5s3$sak_beidz[c(2,4,5)] == "1")) {
            x5s3_uzVieniniekiem <- rbind(x5s3_uzVieniniekiem, x5s3[c(1, 5), ])
            x5s3_uzDivniekiem <- rbind(x5s3_uzDivniekiem, x5s3[2:3, ])
          } else if (all(x5s3$sak_beidz[c(2,3,5)] == "1")) {
            x5s3_uzVieniniekiem <- rbind(x5s3_uzVieniniekiem, x5s3[c(1, 5), ])
            x5s3_uzDivniekiem <- rbind(x5s3_uzDivniekiem, x5s3[3:4, ])
          } else if (all(x5s3$sak_beidz[2:4] == "1")) {
            x5s3_uzVieniniekiem <- rbind(x5s3_uzVieniniekiem, x5s3[1, ])
            x5s3_uzDivniekiem <- rbind(x5s3_uzDivniekiem, x5s3[4:5, ])
          } else if (all(x5s3$sak_beidz[c(1,3,5)] == "1")) {
            x5s3_uzVieniniekiem <- rbind(x5s3_uzVieniniekiem, x5s3[5, ])
            x5s3_uzDivniekiem <- rbind(x5s3_uzDivniekiem, x5s3[1:4, ])
          } else if (all(x5s3$sak_beidz[c(1,3,4)] == "1")) {
            x5s3_uzCetriniekiem <- rbind(x5s3_uzCetriniekiem, x5s3[c(1,2,4,5),])
          } else if (all(x5s3$sak_beidz[c(1,2,4)] == "1")) {
            #Man te izskatās pēc kļūdas. Pagaidām nevispārināšu.
            #Te ir šāda kodu kombinācija: "1", "1", "2", "1", "2". 
            #Pēc dziļākas izpētes man izskatās, ka pirmajam būtu jābūt kodam 2 ne 1. Tā arī rēķinu.
            if (x5s3$PS_code[1] == '____________' && x5s3$NM_code[1] == '____________') {
              x5s3[1, 'sak_beidz'] <- "2"
              x5s3[1, 'zinkod'] <- "50"
              x5s3_uzVieniniekiem <- rbind(x5s3_uzVieniniekiem, x5s3[1,])
              x5s3_uzCetriniekiem <- rbind(x5s3_uzCetriniekiem, x5s3[2:5,])
            } else {stop("processingFives_s3 iztrūkst apstrādes koda")}
          } else {stop("processingFives_s3 iztrūkst apstrādes koda")}
} else if (all(diff(x5s3$NDZ_sanemsanas_datums[1:3]) != 0) && diff(x5s3$NDZ_sanemsanas_datums[4:5]) == 0) {
          if (all(x5s3$sak_beidz[c(1, 3, 5)] == "1")) {
            x5s3_uzVieniniekiem <- rbind(x5s3_uzVieniniekiem, x5s3[5, ])
            x5s3_uzDivniekiem <- rbind(x5s3_uzDivniekiem, x5s3[1:4, ])
          } else if (all(x5s3$sak_beidz[c(1, 3, 4)] == "1")) {
            if (x5s3$PS_code[1] == '____________' && x5s3$NM_code[1] == '____________') {
              x5s3_uzDivniekiem <- rbind(x5s3_uzDivniekiem, x5s3[c(1:3,5), ])
            } else if (x5s3$PS_code[1] == '____________' && x5s3$NM_code[1] == '____________') {
              x5s3_uzDivniekiem <- rbind(x5s3_uzDivniekiem, x5s3[c(1:3,5), ])
            } else {stop("processingFives_s3 iztrūkst apstrādes koda")}
          } else {stop("processingFives_s3 iztrūkst apstrādes koda")}
} else if (diff(x5s3$NDZ_sanemsanas_datums[2:3]) == 0 && 
           all(sapply(c(1,3,4), function(i) diff(x5s3$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
        if (all(x5s3$sak_beidz[c(1,3,5)] == "1")) {
          if((x5s3$PS_code[1] == "____________" && x5s3$NM_code[1] == "____________") ||
             (x5s3$PS_code[1] == "____________" && x5s3$NM_code[1] == "____________")) {
            x5s3_uzVieniniekiem <- rbind(x5s3_uzVieniniekiem, x5s3[5, ])
            x5s3_uzCetriniekiem <- rbind(x5s3_uzCetriniekiem, x5s3[-5, ])
          } else {stop("processingFives_s3 iztrūkst apstrādes koda")}
        } else if (all(x5s3$sak_beidz == c("2", "2", "1", "1", "1"))) {
          if(x5s3$PS_code[1] == "____________" && x5s3$NM_code[1] == "____________") {
            x5s3_uzVieniniekiem <- rbind(x5s3_uzVieniniekiem, x5s3[1, ])
          } else {stop("processingFives_s3 iztrūkst apstrādes koda")}
        } else {stop("processingFives_s3 iztrūkst apstrādes koda")}
} else if (diff(x5s3$NDZ_sanemsanas_datums[2:3]) == 0 &&
           all(sapply(c(1,3), function(i) x5s3$NDZ_sanemsanas_datums[i:(i+1)] != 0))) {
          if (all(x5s3$sak_beidz[1:3] == "1")) {
            x5s3_uzDivniekiem <- rbind(x5s3_uzDivniekiem, x5s3[c(3,5), ])
          } else {stop("processingFives_s3 iztrūkst apstrādes koda")}
} else if (all(sapply(c(1,2,4), function(i) x5s3$NDZ_sanemsanas_datums[i:(i+1)] == 0)) && 
           diff(x5s3$NDZ_sanemsanas_datums[3:4]) != 0) {
          if (all(x5s3$sak_beidz[3:5] == "1")) {
            x5s3_uzVieniniekiem <- rbind(x5s3_uzVieniniekiem, x5s3[c(3,5), ])
          } else {stop("processingFives_s3 iztrūkst apstrādes koda")}
} else if (all(diff(x5s3$NDZ_sanemsanas_datums[1:3]) == 0) && all(diff(x5s3$NDZ_sanemsanas_datums[3:5]) != 0)) {
          if (all(x5s3$sak_beidz[3:5] == "1")) {
            x5s3_uzVieniniekiem <- rbind(x5s3_uzVieniniekiem, x5s3[c(1,5), ])
          } else if (all(x5s3$sak_beidz[c(1,4,5)] == "1") && !("26" %in% x5s3$zinkod)) {
            x5s3_uzVieniniekiem <- rbind(x5s3_uzVieniniekiem, x5s3[5, ])
            x5s3_uzDivniekiem <- rbind(x5s3_uzDivniekiem, x5s3[c(1,3), ])
          } else {stop("processingFives_s3 iztrūkst apstrādes koda")}
} else if (all(sapply(c(2,4), function(i) diff(x5s3$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) && 
           all(sapply(c(1,3), function(i) diff(x5s3$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
          if (all(x5s3$sak_beidz[c(1,4,5)] == "1")){
            x5s3_uzVieniniekiem <- rbind(x5s3_uzVieniniekiem, x5s3[5, ])
            x5s3_uzDivniekiem <- rbind(x5s3_uzDivniekiem, x5s3[c(1, 3), ])
          } else {stop("processingFives_s3 iztrūkst apstrādes koda")}
} else if (all(diff(x5s3$NDZ_sanemsanas_datums[1:3]) == 0) && 
             all(diff(x5s3$NDZ_sanemsanas_datums[3:5]) != 0)) {
          if (!("26" %in% x5s3$zinkod[x5s3$sak_beidz == "2"])) {
           x5s3_uzVieniniekiem <- rbind(x5s3_uzVieniniekiem, x5s3[5, ])
           x5s3_uzDivniekiem <- rbind(x5s3_uzDivniekiem, x5s3[c(1, 3), ])
          } else {stop("processingFives_s3: Iztrūkst apstrādes koda!\n")}
} else if (all(diff(x5s3$NDZ_sanemsanas_datums[2:4]) != 0) &&
           all(sapply(c(1,4), function(i) diff(x5s3$NDZ_sanemsanas_datums[i:(i+1)]) == 0))) {
           if (all(x5s3$sak_beidz[3:5] == "1")) {
            x5s3_uzVieniniekiem <- rbind(x5s3_uzVieniniekiem, x5s3[c(1, 5), ])
          } else {stop("processingFives_s3: Iztrūkst apstrādes koda!\n")}
} else if (diff(x5s3$NDZ_sanemsanas_datums[3:4]) == 0 &&
           all(sapply(c(1,2,4), function(i) diff(x5s3$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
          if (all(x5s3$sak_beidz == c("2", "1", "2", "1", "1"))) {
            x5s3_uzVieniniekiem <- rbind(x5s3_uzVieniniekiem, x5s3[c(1,5),])
            x5s3_uzDivniekiem <- rbind(x5s3_uzDivniekiem, x5s3[2:3,])
          } else {stop("processingFives_s3: Iztrūkst apstrādes koda!\n")}
} else if (diff(x5s3$NDZ_sanemsanas_datums[1:2]) == 0 &&
           all(sapply(2:4, function(i) diff(x5s3$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
            if (all(x5s3$sak_beidz == c("2", "1", "1", "2", "1"))) {
              x5s3_uzVieniniekiem <- rbind(x5s3_uzVieniniekiem, x5s3[5,])
              x5s3_uzDivniekiem <- rbind(x5s3_uzDivniekiem, x5s3[c(2,1,3,4),])
            } else if (all(x5s3$sak_beidz == c("2", "1", "2", "1", "1"))) {
              if (x5s3$PS_code[1] == '____________' && x5s3$NM_code[1] == '____________') {
                x5s3_uzVieniniekiem <- rbind(x5s3_uzVieniniekiem, x5s3[5,])
                x5s3_uzDivniekiem <- rbind(x5s3_uzDivniekiem, x5s3[2:3,])
              } else {stop("processingFives_s3: Iztrūkst apstrādes koda!\n")}
            } else {stop("processingFives_s3: Iztrūkst apstrādes koda!\n")}
} else if (all(sapply(c(1,3), function(i) diff(x5s3$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
           all(sapply(c(2,4), function(i) diff(x5s3$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
            if (all(x5s3$sak_beidz == c("2", "1", "2", "1", "1"))) {
              if(x5s3$PS_code[1] %in% c('____________', '____________') && x5s3$NM_code[1] %in% c('____________', '____________')) {
                x5s3_uzVieniniekiem <- rbind(x5s3_uzVieniniekiem, x5s3[5,])
                x5s3_uzDivniekiem <- rbind(x5s3_uzDivniekiem, x5s3[c(2,1,4,3),])
              } else if (x5s3$PS_code[1] == '____________' && x5s3$NM_code[1] == '____________') {
                x5s3_uzVieniniekiem <- rbind(x5s3_uzVieniniekiem, x5s3[c(1,5),])
                x5s3_uzDivniekiem <- rbind(x5s3_uzDivniekiem, x5s3[c(4,3),])
              } else {stop("processingFives_s3: Iztrūkst apstrādes koda!\n")}
            } else if (all(x5s3$sak_beidz == c("2", "1", "1", "2", "1"))) {
              if(x5s3$PS_code[1] %in% c('____________') && x5s3$NM_code[1] %in% c('____________')) {
                x5s3_uzVieniniekiem <- rbind(x5s3_uzVieniniekiem, x5s3[5,])
                x5s3_uzDivniekiem <- rbind(x5s3_uzDivniekiem, x5s3[c(2,1,3,4),])
              } else {stop("processingFives_s3: Iztrūkst apstrādes koda!\n")}
            } else if (all(x5s3$sak_beidz == c("1", "2", "2", "1", "1"))) {
              if(x5s3$PS_code[1] == '____________' && x5s3$NM_code[1] == '____________' && x5s3$period[1] == "____________") {
                x5s3_uzVieniniekiem <- rbind(x5s3_uzVieniniekiem, x5s3[5,])
                x5s3_uzDivniekiem <- rbind(x5s3_uzDivniekiem, x5s3[c(1,2,4,3),])
              } else {stop("processingFives_s3: Iztrūkst apstrādes koda!\n")}
            } else {stop("processingFives_s3: Iztrūkst apstrādes koda!\n")}
} else {stop("processingFives_s3: Iztrūkst apstrādes koda!\n")}
  
  
  
rm(x5s3)
return(list(x5_uzVieniniekiem = x5s3_uzVieniniekiem, 
            x5_uzDivniekiem = x5s3_uzDivniekiem, 
            x5_uzCetriniekiem = x5s3_uzCetriniekiem))
}

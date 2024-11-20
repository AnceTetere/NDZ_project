processingFives <- function(x, o) {
  x <- x %>% arrange(PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
  x5_uzVieniniekiem <- data.frame(); x5_uzDivniekiem <- data.frame(); x5_uzCetriniekiem <- data.frame()

  fncResult <- function(result) {
    if (exists("result")) {
      x5_uzVieniniekiem <<- rbind(x5_uzVieniniekiem, result$x5_uzVieniniekiem)
      x5_uzDivniekiem <<- rbind(x5_uzDivniekiem, result$x5_uzDivniekiem)
      x5_uzCetriniekiem <<- rbind(x5_uzCetriniekiem, result$x5_uzCetriniekiem)
    }
    rm(result)}
  
  for (r in seq(1, nrow(x), by = 5)) {
    x5 <- x[r:(r+4),] %>% arrange(PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
    
    if (sum(x5$sak_beidz == "1") == 5) {
          x_vieninieki <- codes_match(x5)
          x5_uzVieniniekiem <- rbind(x5_uzVieniniekiem, x_vieninieki)
          rm(x_vieninieki)
    } else if (sum(x5$sak_beidz == "1") == 4) {
           if (all(x5$sak_beidz == c("1", "1", "2", "1", "1"))) {
               if(all(diff(x5$NDZ_sanemsanas_datums) != 0)) {
                 x5_uzVieniniekiem <- rbind(x5_uzVieniniekiem, x5[5,])
                 x5_uzDivniekiem <- rbind(x5_uzDivniekiem, x5[2:3,])
               } else if(diff(x5$NDZ_sanemsanas_datums[3:4]) == 0 && all(sapply(c(1,2,4), function(i) diff(x5$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
                 x5_uzVieniniekiem <- rbind(x5_uzVieniniekiem, x5[5,])
                 x5_uzDivniekiem <- rbind(x5_uzDivniekiem, x5[2:3,])
               } else {stop("processingFives: Tabula nepārdalījās; rinda: ", r, ".\n")}
            } else if (all(x5$sak_beidz == c("1", "1", "1", "1", "2"))) {
                x5_uzDivniekiem <- rbind(x5_uzDivniekiem, x5[4:5,])
            } else {stop("processingFives: Tabula nepārdalījās; rinda: ", r, ".\n")}
    } else if (sum(x5$sak_beidz == "1") == 3) {
          x5 %>% 
            processingFives_s3() %>% 
            fncResult()
    } else if (sum(x5$sak_beidz == "1") == 2) {
          x5 %>% processingFives_s2() %>% fncResult()
    } else if (sum(x5$sak_beidz == "1") == 1) {
        if (all(x5$sak_beidz == c("2", "1", "2", "2", "2"))) {
          if (all(diff(x5$NDZ_sanemsanas_datums) != 0)){
            if ((x5$PS_code[1] %in% c('__________', '__________') && x5$NM_code[1] == '__________') ||
                (x5$PS_code[1] == '__________' && x5$NM_code[1] == '__________')) {
              x5_uzVieniniekiem <- rbind(x5_uzVieniniekiem, x5[1,])
              x5_uzDivniekiem <- rbind(x5_uzDivniekiem, x5[c(2,5),])
            } else {stop("processingFives: Tabula nepārdalījās; rinda: ", r, ".\n")}
          } else if (diff(x5$NDZ_sanemsanas_datums[1:2]) == 0 && all(diff(x5$NDZ_sanemsanas_datums[2:5]) != 0)) {
            if (x5$PS_code[1] == '__________' && x5$NM_code[1] == '__________') {
              x5_uzVieniniekiem <- rbind(x5_uzVieniniekiem, x5[1,])
              x5_uzDivniekiem <- rbind(x5_uzDivniekiem, x5[c(2,5),])
            } else {stop("processingFives: Tabula nepārdalījās; rinda: ", r, ".\n")}
          } else {stop("processingFives: Tabula nepārdalījās; rinda: ", r, ".\n")}
        } else if (all(x5$sak_beidz == c("2", "2", "2", "2", "1"))) {
          if (all(diff(x5$NDZ_sanemsanas_datums) != 0)) {
            x5_uzVieniniekiem <- rbind(x5_uzVieniniekiem, x5[4:5,])
          } else {stop("processingFives: Tabula nepārdalījās; rinda: ", r, ".\n")}
      } else {stop("processingFives: Tabula nepārdalījās; rinda: ", r, ".\n")}
    } else {stop("processingFives: Tabula nepārdalījās; rinda: ", r, ".\n")}
  }    
  rm(x, r, x5, fncResult)
  
#1 Apakštabulu x5_uzVieniniekiem apstrādā caur funkciju processingOnes().
if(nrow(x5_uzVieniniekiem) > 0) {
    x5_uzVieniniekiem %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingOnes(o) %>% sendTo_tempNDZ()
} else {cat("Tabula x5_uzVieniniekiem ir tukša.\n")}
rm(x5_uzVieniniekiem)
  
#2 Apakštabulu x5_uzDivniekiem apstrādā caur funkciju processingTwoes().
if(nrow(x5_uzDivniekiem) > 0) {
    x5_uzDivniekiem %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingTwoes(o)
} else {cat("Tabula x5_uzDivniekiem ir tukša.\n")}
rm(x5_uzDivniekiem)
  
#3 Apakštabulu x5_uzCetriniekiem sūta caur funkciju processingFours().
if(nrow(x5_uzCetriniekiem > 0)) {
    x5_uzCetriniekiem %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingFours(o)
} else {cat("Tabula x5_uzCetriniekiem ir tukša.\n")}
rm(x5_uzCetriniekiem) 
}

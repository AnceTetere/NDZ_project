processingEights_s3 <- function(x8s3, o, kods) {
  x8s3 <- x8s3 %>% arrange(PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
  #x8s3 <- x8
  x8s3_uzVieniniekiem <- data.frame(); x8s3_uzDivniekiem <- data.frame(); x8s3_uzCetriniekiem <- data.frame()
  x8s3_uz5 <- data.frame(); x8s3_uzSesi <- data.frame(); x8s3_uzSeptini <- data.frame()
  
 #result <- function(x) {
 #  x8s3_uzVieniniekiem <<- x$x8s3_uzVieniniekiem
 #  x8s3_uzDivniekiem <<- x$x8s3_uzDivniekiem
 #  x8s3_uzCetriniekiem <<- x$x8s3_uzCetriniekiem
 #  x8s3_uz5 <<- x$x8s3_uz5
 #  x8s3_uzSesi <<- x$x8s3_uzSesi
 #  x8s3_uzSeptini <<- x$x8s3_uzSeptini
 #  rm(x)}
  
 if (x8s3$sak_beidz[1] == "2") {
   if (x8s3$sak_beidz[2] == "1") {
     if (x8s3$sak_beidz[3] == "2") {
       if (x8s3$sak_beidz[4] == "1") {
         if (all(x8s3$sak_beidz[5:8] == c("2","2","1","2")) && 
                diff(x8s3$NDZ_sanemsanas_datums[1:2]) == 0 && 
                diff(x8s3$NDZ_sanemsanas_datums[2:3]) != 0 &&
                x8s3$PS_code[1] ==  '______________' && x8s3$NM_code[1] ==  '______________') {
                 #Nezinu, vai šo var vispārināt.
                  x8s3_uzDivniekiem <- x8s3[c(2,3,4,6,7,8), ]
          } else {stop("processingEights_s3: Iztrūkst kods. \n")}
       } else if (x8s3$sak_beidz[4] == "2") {
         if (all(x8s3$sak_beidz[5:8] == c("1","2","1","2")) && 
             all(diff(x8s3$NDZ_sanemsanas_datums) != 0) &&
             x8s3$period[1] == '______' && x8s3$PS_code[1] ==  '______________' && x8s3$NM_code[1] ==  '______________') {
              #JO PIRMOREIZ
              x8s3_uzVieniniekiem <- x8s3[1,];x8s3_uzSeptini <- x8s3[2:8,]
         } else {stop("processingEights_s3: Iztrūkst kods. \n")}
       } else {stop("processingEights_s3: Iztrūkst kods. \n")}
     } else {stop("processingEights_s3: Iztrūkst kods. \n")}
   } else {stop("processingEights_s3: Iztrūkst kods. \n")}
} else if (x8s3$sak_beidz[1] == "1") {
        if (all(x8s3$sak_beidz[2:4] == c("2", "2", "1"))) { 
          if (all(sapply(c(2,3,5,6), function(i) diff(x8s3$NDZ_sanemsanas_datums[i:(i+1)]) != 0)) &&
              all(sapply(c(1,4,7), function(i) diff(x8s3$NDZ_sanemsanas_datums[i:(i+1)]) == 0))) {
               #JO PIRMOREIZ
               if (x8s3$period[1] == '______' && x8s3$PS_code[1] ==  '______________' && x8s3$NM_code[1] ==  '______________') {
               x8s3_uzDivniekiem <- x8s3[c(1,3), ]; x8s3_uz5 <- x8s3[4:8,]
             } else {stop("processingEights_s3: Iztrūkst kods. \n")}
          } else {stop("processingEights_s3: Iztrūkst kods.\n")}
         } else if (all(x8s3$sak_beidz[2:4] == c("1", "1", "2"))) {
           if (all(diff(x8s3$NDZ_sanemsanas_datums[1:4]) != 0)) {
               #JO PIRMOREIZ
               if (x8s3$period[1] == '______' && x8s3$PS_code[1] ==  '______________' && x8s3$NM_code[1] ==  '______________') {
                x8s3_uzSesi <- rbind(x8s3_uzSesi, x8s3[3:8, ])
               } else {stop("processingEights_s3: Iztrūkst kods. \n")}
            } else {stop("processingEights_s3: Iztrūkst kods. \n")}
         } else if (all(x8s3$sak_beidz[2:5] == c("2", "2", "2", "1"))) {
                 if (all(sapply(c(1,4), function(i) diff(x8s3$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
                     all(diff(x8s3$NDZ_sanemsanas_datums[2:4]) != 0)) {
                     #JO PIRMOREIZ
                     if (x8s3$period[1] == '______' && x8s3$PS_code[1] ==  '______________' && x8s3$NM_code[1] ==  '______________') {
                         x8s3_uzDivniekiem <- x8s3[c(1,3), ]; x8s3_uz5 <- x8s3[c(5,4,6,8,7),]
                         if (kods %in% c("40", "50", "53") && o == "8") {ZERO_minus(x8s3 %>% slice(1))}
                     } else {stop("processingEights_s3: Iztrūkst kods. \n")}
                 } else {stop("processingEights_s3: Iztrūkst kods. \n")}
         } else if (all(x8s3$sak_beidz[2:8] == c("2", "2", "2", "2", "2", "1", "1"))) {
                  if (all(diff(x8s3$NDZ_sanemsanas_datums) != 0)) {
                      #JO PIRMOREIZ
                       if (x8s3$period[1] == '______' && x8s3$PS_code[1] ==  '______________' && x8s3$NM_code[1] ==  '______________') {
                            x8s3_uzDivniekiem <- x8s3[c(1,6), ]; x8s3_uz1 <- x8s3[8,]
                             if (kods %in% c("40", "50", "53") && o == "8") {ZERO_minus(x8s3 %>% slice(1))}
                       } else {stop("processingEights_s3: Iztrūkst kods. \n")}
                  } else {stop("processingEights_s3: Iztrūkst kods. \n")}
         } else {stop("processingEights_s3: Iztrūkst kods. \n")}
} else {stop("processingEights_s3: Iztrūkst kods. \n")}
  
rm(x8s3, o, kods)
return(list(x8_uzVieniniekiem = x8s3_uzVieniniekiem, 
              x8_uzDivniekiem = x8s3_uzDivniekiem, 
              x8_uzCetriniekiem = x8s3_uzCetriniekiem, 
              x8_uz5 = x8s3_uz5, 
              x8_uzSesi = x8s3_uzSesi, 
              x8_uzSeptini = x8s3_uzSeptini))
}

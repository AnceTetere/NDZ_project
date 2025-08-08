processingTens_s4 <- function(x10s4, o, kods) {
  x10s4_1 <- data.frame(); x10s4_2 <- data.frame(); x10s4_5 <- data.frame(); x10s4_6 <- data.frame(); x10s4_7 <- data.frame(); x10s4_8 <- data.frame()
  #x10s4 <- x10
  
  if (all(sapply(seq(1, 10, by = 2), function(i) diff(x10s4$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
      all(sapply(seq(2, 8, by = 2), function(i) diff(x10s4$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
              if (all(x10s4$sak_beidz[c(1, 2)] == "2") && all(x10s4$zinkod[c(1, 2)] == "26")) {
                x10s4_8 <- x10s4[3:10, ]
              } else if (all(x10s4$sak_beidz[c(1,2,5,6,9,10)] == "2") && all(x10s4$sak_beidz[c(3,4,7,8)] == "1") && "91" %in% x10s4$zinkod) {
                x10s4_1 <- x10s4[2,]; x10s4_2 <- x10s4[c(3,5,7,9), ]
              } else {stop("processingTens: Desmitnieku tabulas pārdalei trūkst izstrādes koda. \n")}
            } else if (all(x10s4$sak_beidz[c(3:4,7,9)] == "1") && 
                       all(sapply(seq(1,10,by=2), function(i) all(diff(x10s4$NDZ_sanemsanas_datums[i:(i+1)]) == 0))) &&
                       all(sapply(seq(2,9,by=2), function(i) all(diff(x10s4$NDZ_sanemsanas_datums[i:(i+1)]) != 0)))&&
                       x10s4$PS_code[1] ==  '______________' & x10s4$NM_code[1] ==  '______________') {
                         p <- x10s4[1:6, ]
                         p <- p[p$zinkod %in% c("40", "41"), ]
                         x10s4_1 <- p[1, ]; x10s4_2 <- p[2:3, ]; rm(p)
  } else if (all(diff(x10s4$NDZ_sanemsanas_datums) != 0)) {
               if (all(x10s4$sak_beidz[c(2,5,8,10)] == "1")) {
                 
                 if (all(x10s4$sak_beidz[c(2,5,8,10)] == "1")) {
                   if (x10s4$period[1] ==  '______' && x10s4$PS_code[1] ==  '______________' && x10s4$NM_code[1] ==  '______________') {
                     x10s4_1 <- x10s4[c(1,10), ]; x10s4_2 <- x10s4[c(2,4,5,7,8,9), ]
                   } else {stop("processingTens: Trūkst izstrādes koda. \n")}}
    } else {stop("processingTens: Trūkst izstrādes koda. \n")}}
  
  rm(x10s4, o, kods)
  
  return(list(x10_uzVieniniekiem = x10s4_1, 
              x10_uzDivniekiem = x10s4_2, 
              x10_uzPieci = x10s4_5, 
              x10_uzSesi = x10s4_6, 
              x10_uzSeptini = x10s4_7,
              x10_uzAstoniekiem = x10s4_8))
}

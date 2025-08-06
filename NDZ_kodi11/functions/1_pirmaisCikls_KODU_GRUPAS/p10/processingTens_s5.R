processingTens_s5 <- function(x10s5, o, kods) {
  x10s5_1 <- data.frame(); x10s5_2 <- data.frame(); x10s5_5 <- data.frame(); x10s5_6 <- data.frame(); x10s5_7 <- data.frame(); x10s5_8 <- data.frame()
  #x10s5 <- x10
  
  result <- function(y) {
    x10s5_1 <<- rbind(x10s5_1, y$x10s5_1)
    x10s5_2 <<- rbind(x10s5_2, y$x10s5_2)
    x10s5_5 <<- rbind(x10s5_5, y$x10s5_5)
    x10s5_6 <<- rbind(x10s5_6, y$x10s5_6)
    x10s5_7 <<- rbind(x10s5_7, y$x10s5_7)
    x10s5_8 <<- rbind(x10s5_8, y$x10s5_8)
    rm(y)}
  
  if (all(x10s5$sak_beidz[1:2] == c("1","2"))) {
          result(processingTens_s5e12(x10s5, o, kods))
  } else if (all(x10s5$sak_beidz[1:2] == c("2","1"))) {
          result(processingTens_s5e21(x10s5, o, kods))
  } else if (all(x10s5$sak_beidz[1:2] == c("2","2"))) {
          if (all(x10s5$sak_beidz[3:5] == c("1","2", "1"))) {
            if (all(sapply(c(1,3), function(i) diff(x10s5$NDZ_sanemsanas_datums[i:(i+1)]) != 0)) && diff(x10s5$NDZ_sanemsanas_datums[2:3]) == 0) {
              if (x10s5$period[1] == '______' && x10s5$PS_code[1] ==  '______________' && x10s5$NM_code[1] ==  '______________') {
                x10s5 <- x10s5[c(1,3,2,5,4,6,7,9,8,10),]; x10s5_1 <- x10s5[c(1,10), ]; x10s5_8 <- x10s5[2:9,]
             } else {stop("processingTens_s5: Desmitnieku tabulas pārdalei trūkst izstrādes koda\n")}
            } else {stop("processingTens_s5: Desmitnieku tabulas pārdalei trūkst izstrādes koda\n")}
          } else {stop("processingTens_s5: Desmitnieku tabulas pārdalei trūkst izstrādes koda\n")}
  } else {stop("processingTens_s5: Desmitnieku tabulas pārdalei trūkst izstrādes koda\n")} 
  
  rm(x10s5, o, kods)
    
  return(list(x10_uzVieniniekiem = x10s5_1, 
              x10_uzDivniekiem = x10s5_2, 
              x10_uzPieci = x10s5_5, 
              x10_uzSesi = x10s5_6, 
              x10_uzSeptini = x10s5_7,
              x10_uzAstoniekiem = x10s5_8))
}

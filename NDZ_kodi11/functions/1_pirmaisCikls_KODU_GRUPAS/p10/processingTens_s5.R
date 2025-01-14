processingTens_s5 <- function(x10s5) {
  
  x10s5_1 <- data.frame(); x10s5_2 <- data.frame(); x10s5_7 <- data.frame(); x10s5_8 <- data.frame()
  # x10s5 <- x10
  
  if (all(x10s5$sak_beidz[1:2] == c("1","2")) || (all(x10s5$sak_beidz[1:2] == c("2","1")) && diff(x10s5$NDZ_sanemsanas_datums[1:2]) == 0)) {
               x10s5_2 <- rbind(x10s5_2, x10s5[1:2, ])
               x10s5_8 <- rbind(x10s5_8, x10s5[3:10, ])
  } else if (all(x10s5$sak_beidz[1:4] == c("2", "1", "2", "1")) && 
             diff(x10s5$NDZ_sanemsanas_datums[1:2]) != 0) {
                x10s5_1 <- rbind(x10s5_1, x10s5[1, ])
                x10s5_2 <- rbind(x10s5_2, x10s5[2:3, ])
                x10s5_7 <- rbind(x10s5_7, x10s5[4:10, ])
  } else if (all(x10s5$sak_beidz[c(1, 3, 4, 6, 8)] == "2") && 
             all(x10s5$sak_beidz[c(2, 5, 7, 9, 10)] == "1") && 
             all(sapply(seq(4, 9, by = 2), function(i) diff(x10s5$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
             all(sapply(seq(1, 4, by = 2), function(i) diff(x10s5$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
               x10s5_1 <- rbind(x10s5_1, x10s5[1, ])
               x10s5_2 <- rbind(x10s5_2, x10s5[2:3, ])
               x10s5_7 <- rbind(x10s5_7, x10s5[4:10, ])
    #} else if (all(x10s5$sak_beidz[c(1, 2, 5, 6, 8)] == "2") && 
    #           all(x10s5$sak_beidz[c(3, 4, 7, 9, 10)] == "1") && 
    #           all(sapply(seq(6, 9, by = 2), function(i) all(diff(x10s5$NDZ_sanemsanas_datums[i:(i+1)]) == 0))) &&
    #           all(sapply(seq(3, 6, by = 2), function(i) all(diff(x10s5$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) &&
    #           all(diff(x10$NDZ_sanemsanas_datums[2:3]) == 0) &&
    #           all(diff(x10$NDZ_sanemsanas_datums[9:10]) != 0)) {
    #  x10s5_1 <- rbind(x10s5_1, x10[1, ])
    #  x10s5_2 <- rbind(x10s5_2, x10[2:3, ])
    #  x10s5_7 <- rbind(x10s5_7, x10[4:10, ])
  } else if (all(x10s5$sak_beidz[c(2, 5, 6, 9, 10)] == "1") && 
             all(x10s5$sak_beidz[c(1, 3, 4, 7, 8)] == "2") && 
             all(sapply(seq(4, 9, by = 2), function(i) diff(x10s5$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
             all(sapply(c(1,3,9), function(i) diff(x10s5$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
                x10s5_1 <- rbind(x10s5_1, x10s5[1, ])
                x10s5_2 <- rbind(x10s5_2, x10s5[2:3, ])
                x10s5_7 <- rbind(x10s5_7, x10s5[4:10, ])
  } else if (all(x10s5$sak_beidz[c(2, 3, 6, 8, 9)] == "1") && 
             all(x10s5$sak_beidz[c(1, 4, 5, 7, 10)] == "2") && 
             all(diff(x10s5$NDZ_sanemsanas_datums) != 0)) {
                x10s5_1 <- rbind(x10s5_1, x10s5[1, ])
                x10s5_2 <- rbind(x10s5_2, x10s5[c(3,5,6,7,9,10), ])
  } else {stop("processingTens_s5: Desmitnieku tabulas pārdalei trūkst izstrādes koda\n")} 
  
  
  return(list(x10_uzVieniniekiem = x10s5_1, 
              x10_uzDivniekiem = x10s5_2, 
              x10_uzSeptini = x10s5_7,
              x10_uzAstoniekiem = x10s5_8))
}

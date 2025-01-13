processingFours_2112 <- function(a) {
  a4_1 <- data.frame(); a4_2 <- data.frame(); a4_3 <- data.frame()
  
  if (diff(a$NDZ_sanemsanas_datums[1:2]) > 0) {
    a4_1 <- rbind(a4_1, a[1, ])
    a4_2 <- rbind(a4_2, a[3:4, ])
  } else if (diff(a$NDZ_sanemsanas_datums[1:2]) == 0 && all(diff(a$NDZ_sanemsanas_datums[2:4]) != 0)){
    a4_2 <- rbind(a4_2, a[c(2,1,3,4), ])
  } else if (all(sapply(c(1,3), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) && diff(a$NDZ_sanemsanas_datums[2:3]) != 0) {
    a4_2 <- rbind(a4_2, a[c(2,1,3,4), ])
  } else {stop("processingFours_2112 trūkst apstrādes koda.")}
  
  return(list(x4_uzVieniniekiem = a4_1,
              x4_trueDoubles = a4_2,
              x4_uzTrijniekiem = a4_3))
}

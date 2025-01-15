processingFours_1122 <- function(a) {
  a4_1 <- data.frame(); a4_2 <- data.frame(); a4_3 <- data.frame()
  
  if (diff(a$NDZ_sanemsanas_datums[2:3]) == 0 && 
      all(sapply(c(1,3), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) != 0)) && 
      a$PS_code[1] == '__________' && a$NM_code[1] == '___________') {
      a4_2 <- rbind(a4_2, a[c(1,3,2,4), ])
  } else if (all(diff(a$NDZ_sanemsanas_datums) != 0)) {
          if (a$period[1] == "__________" && a$PS_code[1] %in% c('__________', '____________') && a$NM_code[1] == '__________') {
            a4_2 <- rbind(a4_2, a[c(2,4), ])
          } else {stop("processingFours_1122 trūkst apstrādes koda.")}
  } else {stop("processingFours_1122 trūkst apstrādes koda.")}

  return(list(x4_uzVieniniekiem = a4_1,
              x4_trueDoubles = a4_2,
              x4_uzTrijniekiem= a4_3))
}

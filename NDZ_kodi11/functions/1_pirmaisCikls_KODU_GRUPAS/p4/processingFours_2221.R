processingFours_2221 <- function(a, o, kods) {
  a4_1 <- data.frame(); a4_2 <- data.frame(); a4_3 <- data.frame()
  #a <- x4
  
 if (all(diff(a$NDZ_sanemsanas_datums) != 0)) {
      #JO PIRMOREIZ.
      if ((a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') ||
          (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') ||
          (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________')) {
        a4_1 <- rbind(a4_1, a[3:4, ])     
      } else {stop("processingFours_2212 trūkst apstrādes koda.")}
 } else {stop("processingFours_2221 trūkst apstrādes koda.")}
  
  return(list(x4_uzVieniniekiem = a4_1,
              x4_trueDoubles = a4_2,
              x4_uzTrijniekiem = a4_3))
}

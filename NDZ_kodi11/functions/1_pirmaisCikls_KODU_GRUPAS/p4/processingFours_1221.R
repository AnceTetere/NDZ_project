processingFours_1221 <- function(a) {
  a4_1 <- data.frame(); a4_2 <- data.frame(); a4_3 <- data.frame()

if (diff(a$NDZ_sanemsanas_datums[1:2]) >= 0 && diff(a$NDZ_sanemsanas_datums[3:4]) > 0) {
  a4_1 <- rbind(a4_1, a[4, ])
  a4_2 <- rbind(a4_2, a[c(1, 3), ])
} else if (diff(a$NDZ_sanemsanas_datums[1:2]) >= 0 && diff(a$NDZ_sanemsanas_datums[2:3]) > 0 && diff(a$NDZ_sanemsanas_datums[3:4]) == 0) {
  a <- arrange(a, NDZ_sanemsanas_datums, sak_beidz) 
  a4_2 <- rbind(a4_2, a)
} else {stop("Šeit četrinieku izstrādes tabulas rindām ", r, " līdz ", r+3, " trūkst apstrādes koda.")}

  return(list(x4_uzVieniniekiem = a4_1,
              x4_trueDoubles = a4_2,
              x4_uzTrijniekiem= a4_3))
}

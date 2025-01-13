processingFours_1211 <- function(a) {
  a4_1 <- data.frame(); a4_2 <- data.frame(); a4_3 <- data.frame()

if (diff(a$NDZ_sanemsanas_datums[1:2]) >= 0) {
  a4_2 <- rbind(a4_2, a[1:2, ])
  a4_1 <- rbind(a4_1, a[4, ])
} else {stop("processingFours_1211 trūkst apstrādes koda.")}

  return(list(x4_uzVieniniekiem = a4_1,
              x4_trueDoubles = a4_2,
              x4_uzTrijniekiem= a4_3))
}

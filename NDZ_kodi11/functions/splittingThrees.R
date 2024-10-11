splittingThrees <- function(x3) {
  
  x_vieninieki <- data.frame()
  x_divnieki <- data.frame()
  
  if (all(x3$sak_beidz == c("1", "2", "2")) && all(x3$zinkod != '26') && 
      all(diff(x3$NDZ_sanemsanas_datums[1:3]) >= 0)) {
      x_divnieki <- rbind(x_divnieki, x3[c(1,3), ])
  } else if (all(x3$sak_beidz == c("1", "2", "1"))) {
    if (diff(x3$NDZ_sanemsanas_datums[1:2]) >= 0 && diff(x3$NDZ_sanemsanas_datums[2:3]) > 0) {
      x_divnieki <- rbind(x_divnieki, x3[1:2, ])
      x_vieninieki <- rbind(x_vieninieki, x3[3, ]) 
    } else if (diff(x3$NDZ_sanemsanas_datums[1:2]) != 0 && diff(x3$NDZ_sanemsanas_datums[2:3]) == 0) {
      # Sekojot lēmumam: ..\manual\lēmumi\kods50_112
      x3 <- arrange(x3, sak_beidz)
      x_divnieki <- rbind(x_divnieki, x3[2:3, ])
    } else {stop("No trijniekiem sūtītajā tabulā uz splittingThrees trūkst apstrādes koda!")}
  } else if (all(x3$sak_beidz == c("2", "1", "2"))) {
    if (diff(x3$NDZ_sanemsanas_datums[1:2]) != 0) {
      x_divnieki <- rbind(x_divnieki, x3[2:3, ])
      x_vieninieki <- rbind(x_vieninieki, x3[1, ])
    } else if (diff(x3$NDZ_sanemsanas_datums[1:2]) == 0 && diff(x3$NDZ_sanemsanas_datums[2:3]) != 0) {
      # Sekojot lēmumam: ..\manual\lēmumi\kods50_122
      x_divnieki <- rbind(x_divnieki, x3[2:3, ])
    } else {stop("No trijniekiem sūtītajā tabulā uz splittingThrees trūkst apstrādes koda!")}
  } else if (all(x3$sak_beidz == c("2", "2", "1")) && all(x3$zinkod != "26")) {
     x_divnieki <- rbind(x_divnieki, x3[2:3, ])
  } else if (all(x3$sak_beidz == c("2", "2", "1")) && any(x3$zinkod %in% "26")) {
     x_divnieki <- rbind(x_divnieki, x3[x3$sak_beidz == "1", ], x3[x3$zinkod == "26", ])
  } else if (all(x3$sak_beidz == c("1", "2", "2")) && any(x3$zinkod %in% '26') && 
             all(diff(x3$NDZ_sanemsanas_datums) >= 0)) {
     x_divnieki <- rbind(x_divnieki, x3[x3$sak_beidz == "1", ], x3[x3$zinkod == "26", ][nrow(x3[x3$zinkod == "26", ]),])
  } else if (all(x3$sak_beidz == c("2", "1", "1")) && diff(x3$NDZ_sanemsanas_datums[c(1,3)]) != 0) {
     x_vieninieki <- rbind(x_vieninieki, x3[c(1, 3), ])
  } else if (all(x3$sak_beidz == c("1", "2", "1")) && 
             diff(x3$NDZ_sanemsanas_datums[1:2]) != 0 && 
             diff(x3$NDZ_sanemsanas_datums[2:3]) == 0 && "41" %in% x3$zinkod) {
    x_vieninieki <- rbind(x_vieninieki, x3[3, ])
    x_divnieki <- rbind(x_divnieki, x3[1:2, ])
  } else {
    stop("No trijniekiem sūtītajā tabulā uz splittingThrees trūkst apstrādes koda!")
  }

  return(list(x_vieninieki = x_vieninieki, x_divnieki = x_divnieki))
}

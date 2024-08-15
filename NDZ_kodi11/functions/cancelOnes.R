cancelOnes <- function(y) {
  
  x_divnieki <- data.frame()
  
  if (diff(y$NDZ_sanemsanas_datums[y$sak_beidz == "1"][1:2]) != 0) {
    x_divnieki <- rbind(x_divnieki, y[c(which.max(y$NDZ_sanemsanas_datums[y$sak_beidz == "1"]), which.max(y$sak_beidz == "2")), ])

  } else if (diff(y$NDZ_sanemsanas_datums[y$sak_beidz == "1"][1:2]) == 0) {
    x_divnieki <- rbind(x_divnieki, y[y$sak_beidz == '1', ][1,], y[y$sak_beidz == "2", ])
  }
  
  return(x_divnieki)
}

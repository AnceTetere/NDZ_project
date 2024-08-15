cancelOnes <- function(y) {
  
  x_divnieki <- data.frame()
  
  if (y$NDZ_sanemsanas_datums[y$sak_beidz == "1"][1] != y$NDZ_sanemsanas_datums[y$sak_beidz == "1"][2]) {
    x_divnieki <- rbind(x_divnieki, y[c(which.max(y$NDZ_sanemsanas_datums[y$sak_beidz == "1"]), which.max(y$end == "2")), ])
  } else if (y$NDZ_sanemsanas_datums[y$sak_beidz == "1"][1] == y$NDZ_sanemsanas_datums[y$sak_beidz == "1"][2]) {
    x_divnieki <- rbind(x_divnieki, y[y$sak_beidz == '1', ][1, ], y[y$end == "2", ])
  }
  return(x_divnieki)
}


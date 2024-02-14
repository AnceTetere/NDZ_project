cancelOnes <- function(y) {
  x_divnieki <- data.frame()

  if (y$sak_darbu[y$start == "1"][1] != y$sak_darbu[y$start == "1"][2]) {
    x_divnieki <- rbind(x_divnieki, y[c(which.max(y$sak_darbu[y$start == "1"]), which.max(y$end == "2")), ])

  } else if (y$sak_darbu[y$start == "1"][1] == y$sak_darbu[y$start == "1"][2]) {
    x_divnieki <- rbind(x_divnieki, y[y$start == '1', ][1, ], y[y$end == "2", ])
  }
  return(x_divnieki)
}

codes_match <- function(y) {
x_viens <- data.frame()
  
  test <- TRUE # test pārbauda datumus ailē [sak]
  test1 <- TRUE # test1 pārbauda datumus ailē [beidz]
  
  for (r in 1:(as.numeric(o) - 1)) {
    a <- (y$sak[r] == y$sak[r + 1])
    if (is.na(a)) {
      a <- FALSE
    }
    test <- test & a
    rm(a)
  }
  
  for (r in 1:(as.numeric(o) - 1)) {
    a <- (y$beidz[r] == y$beidz[r + 1])
    if (is.na(a)) {
      a <- FALSE
    }
    test1 <- test1 & a
    rm(a)
  }
  
  if (test == TRUE || test1 == TRUE) {
    x_viens <- rbind(x_viens, y[1,])
    if (test == TRUE) {
    } else {
    }
    
  } else if (sum(y$start == "1") == as.numeric(o)) {
    for (r in 1:(as.numeric(o) - 1)) {
      b <- max(y$sak[r], y$sak[r + 1])
    }
    x_viens <- rbind(x_viens, y[y$sak == b,])
    rm(b)
  } else if (sum(y$end == "1") == as.numeric(o)) {
    for (r in 1:(as.numeric(o) - 1)) {
      c <- max(y$beidz[r], y$beidz[r + 1])
    }
    x_viens <-
      rbind(x_viens, y[y$beidz == c,])
    rm(c)
  }

  return(x_viens)
}

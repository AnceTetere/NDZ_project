codes_match <- function(y) {
x_viens <- data.frame()

  
  test <- TRUE # test pārbauda datumus ailē [sak_darbu]
  test1 <- TRUE # test1 pārbauda datumus ailē [beidz_darbu]
  
  for (r in 1:(as.numeric(o) - 1)) {
    a <- (y$sak_darbu[r] == y$sak_darbu[r + 1])
    if (is.na(a)) {
      a <- FALSE
    }
    test <- test & a
    rm(a)
  }


  for (r in 1:(as.numeric(o) - 1)) {
    a <- (y$beidz_darbu[r] == y$beidz_darbu[r + 1])
    if (is.na(a)) {
      a <- FALSE
    }
    test1 <- test1 & a
    rm(a)
  }
  
  #3. Attiecīgi iekārto šo vieninieku tabulā
  if (test == TRUE || test1 == TRUE) {
    x_viens <- rbind(x_viens, y[1,])
    if (test == TRUE) {
      cat(
        paste0(
          "Apakštabulā, kurā visi ",
          o,
          " datumi ailē [sak_darbu] sakrīt, tika izvēlēta pirmā, ko sūtīt uz vieninieku apstrādi."
        )
      )
    } else {
      cat(
        paste0(
          "Apakštabulā, kurā visi ",
          o,
          " datumi ailē [beidz_darbu] sakrīt, tika izvēlēta pirmā, ko sūtīt uz vieninieku apstrādi."
        )
      )
    }
    
  } else if (sum(y$start == "1") == as.numeric(o)) {

    for (r in 1:(as.numeric(o) - 1)) {
      b <- max(y$sak_darbu[r], y$sak_darbu[r + 1])
    }

    x_viens <- rbind(x_viens, y[y$sak_darbu == b,])
    cat(
      paste0(
        "Apakštabulā, kurā visi ",
        o,
        " kodi ailē [start] sakrīt, bet datumi ailē [sak_darbu] nesakrīt, \n",
        "tika izvēlēta rinda ar vēlāko datumu ",
        b,
        ", ko sūtīt uz vieninieku apstrādi."
      )
    )
    rm(b)
  } else if (sum(y$end == "1") == as.numeric(o)) {
    #ja beigu kodi ir vienādi, bet datumi ailē [beidz_darbu] atšķiras,
    #tad vieninieku ailē iet rinda ar vēlāko datumu.
    for (r in 1:(as.numeric(o) - 1)) {
      c <- max(y$beidz_darbu[r], y$beidz_darbu[r + 1])
    }
    x_viens <-
      rbind(x_viens, y[y$beidz_darbu == c,])
    cat(
      paste0(
        "Apakštabulā, kurā visi ",
        o,
        " kodi ailē [end] sakrīt, bet datumi ailē [beidz_darbu] nesakrīt, \n",
        "tika izvēlēta rinda ar vēlāko datumu ",
        c,
        ", ko sūtīt uz vieninieku apstrādi."
      )
    )
    rm(c)
  }
  # funkcija atgriež vieninieku apakštabulu
  return(x_viens)
}

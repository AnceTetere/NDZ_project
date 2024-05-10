processingEleven <- function(x, o) {

  x <- x[order(x$PS_code, x$dnperk, x$NM_code, x$NDZ_sanemsanas_datums), ]
  
    x11_uzVieniniekiem <- data.frame()
    x11_uzDevini <- data.frame()
    x11_uzDesmitniekiem <- data.frame()
    check_rows <- 0
  
    for (r in seq(1, nrow(x), by = 11)) {
      x11 <- x[r:(r+10),]
      x11 <- x11[order(x11$PS_code, x11$dnperk, x11$NM_code, x11$NDZ_sanemsanas_datums), ]
      
      if (sum(x11$start == "1") == 6) {
        if (all(x11$start[c(1, 3, 5:6, 8, 10)] == "1") && 
            all(diff(x11$NDZ_sanemsanas_datums[c(1:3, 5:11)]) != 0) &&
            all(diff(x11$NDZ_sanemsanas_datums[4:5]) == 0)) {
          x11_uzDesmitniekiem <- rbind(x11_uzDesmitniekiem, x11[-5,])
        } else if (all(x11$start[c(1, 3, 5, 8:9, 11)] == "1") && 
              all(diff(x11$NDZ_sanemsanas_datums[c(1:2, 4:7, 9:11)]) != 0) &&
              all(sapply(c(3, 7), function(i) all(diff(x11$NDZ_sanemsanas_datums[i:(i+1)]) == 0)))) {
          x11_uzVieniniekiem <- rbind(x11_uzVieniniekiem, x11[11, ])
          x11_uzDesmitniekiem <- rbind(x11_uzDesmitniekiem, x11[-11,])
        } else if (all(x11$start[c(1, 4, 6, 8, 10:11)] == "1") && 
                   all(sapply(c(1, 2, 4, 6, 8, 10), function(i) all(diff(x11$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) && 
                   all(sapply(seq(3, 9, by = 2), function(i) all(diff(x11$NDZ_sanemsanas_datums[i:(i+1)]) == 0)))) {
          x11_uzVieniniekiem <- rbind(x11_uzVieniniekiem, x11[11, ])
          x11_uzDesmitniekiem <- rbind(x11_uzDesmitniekiem, x11[-11,])
        } else if (all(x11$start[c(1, 4, 5, 7, 9, 11)] == "1") && 
                   all(sapply(c(2, 4:6, 8:10), function(i) all(diff(x11$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) && 
                   all(sapply(c(seq(1, 4, by = 2), 7), function(i) all(diff(x11$NDZ_sanemsanas_datums[i:(i+1)]) == 0)))) {
          x11_uzVieniniekiem <- rbind(x11_uzVieniniekiem, x11[11, ])
          x11_uzDesmitniekiem <- rbind(x11_uzDesmitniekiem, x11[-11,])
        } else if (all(x11$start[c(2, 4, 6, 8, 10, 11)] == "1") && all(diff(x11$NDZ_sanemsanas_datums) != 0)) {
          x11_uzVieniniekiem <- rbind(x11_uzVieniniekiem, x11[11, ])
          x11_uzDevini <- rbind(x11_uzDevini, x11[1:9,])
        } else if (all(x11$start[seq(1,11,by=2)] == "1") && all(diff(x11$NDZ_sanemsanas_datums) != 0)) {
          x11_uzVieniniekiem <- rbind(x11_uzVieniniekiem, x11[11, ])
          x11_uzDesmitniekiem <- rbind(x11_uzDesmitniekiem, x11[-11,])
        } else if (all(x11$start[seq(1,11,by=2)] == "1") && 
                   all(sapply(c(1:6,8:10), function(i) diff(x11$NDZ_sanemsanas_datums[i:(i+1)]) != 0)) &&
                   diff(x11$NDZ_sanemsanas_datums[7:8]) == 0) {
          x11_uzVieniniekiem <- rbind(x11_uzVieniniekiem, x11[11, ])
          x11_uzDesmitniekiem <- rbind(x11_uzDesmitniekiem, x11[-11,])
        } else {
          stop("processingEleven: Vienpadsmitnieku tabulas pārdalei trūkst izstrādes koda. Rindas: ", r, " līdz ", r + 10, "\n")
        }
      } else if (sum(x11$end == "2") == 6) {
        if (x11$end[1] == "2" && x11$start[2] == "1" && all(diff(x11$NDZ_sanemsanas_datums[1:2]) != 0)) {
          x11_uzVieniniekiem <- rbind(x11_uzVieniniekiem, x11[1,])
          x11_uzDesmitniekiem <- rbind(x11_uzDesmitniekiem, x11[2:11,])
        } else {
          stop("processingEleven: Vienpadsmitnieku tabulas pārdalei trūkst izstrādes koda. Rindas: ", r, " līdz ", r + 10, "\n")
        }
      } else {
        stop("processingEleven: Vienpadsmitnieku tabulas pārdalei trūkst izstrādes koda. Rindas: ", r, " līdz ", r + 10, "\n")
      }
      check_rows <- check_rows + 11
    }
    
    #2 PĀRBAUDE: Vai rindu skaits no vienpadsmitniekiem atvasinātajās tabulās sakrīt ar rindām izejas tabulā x.
    if (check_rows == nrow(x)) {
      cat("PĀRBAUDE IZIETA: Apakštabulu rindu summa sakrīt ar izejošo vienpadsmitnieku tabulu.\n")
      rm(x, x11, r, check_rows)
    } else {
      stop("processingEleven: PĀRBAUDE NAV IZIETA. Apakštabulu rindu summa NESAKRĪT ar izejošo vienpadsmitnieku tabulu.")
    }
    
    #3 Apakštabulu x11_uzVieniniekiem sūta caur processingOnes().
    if (nrow(x11_uzVieniniekiem) > 0) {
      x11_uzVieniniekiem <-
        x11_uzVieniniekiem[order(
          x11_uzVieniniekiem$PS_code,
          x11_uzVieniniekiem$NM_code,
          x11_uzVieniniekiem$NDZ_sanemsanas_datums
        ),]
      cat(
        "No vienpadsmitniekiem atvasinātā tabula x11_uzVieniniekiem pārsūtīta uz processingOnes un tad uz tempNDZ, ko būvējam."
      )
      sendTo_tempNDZ(processingOnes(x11_uzVieniniekiem, o))
    } else {
      cat("Tabula x11_uzVieniniekiem ir tukša.")
    }
    rm(x11_uzVieniniekiem)
    
    #4 Apakštabulu x11_uzDeviņi sūta caur processingNines().
    if (nrow(x11_uzDevini) > 0) {
      x11_uzDevini <-x11_uzDevini[order(x11_uzDevini$PS_code, x11_uzDevini$NM_code, x11_uzDevini$NDZ_sanemsanas_datums),]
      cat(
        "No vienpadsmitniekiem atvasinātā tabula x11_uzDevini pārsūtīta uz processingNines() un tad uz tempNDZ, ko būvējam."
      )
      processingNines(x11_uzDevini, o)
    } else {
      cat("Tabula x11_uzDevini ir tukša.")
    }
    rm(x11_uzDevini)
    
    
    #5 Apakštabulu x11_uzDesmitniekiem sūta caur processingTens().
    if (nrow(x11_uzDesmitniekiem) > 0) {
      x11_uzDesmitniekiem <-
        x11_uzDesmitniekiem[order(
          x11_uzDesmitniekiem$PS_code,
          x11_uzDesmitniekiem$NM_code,
          x11_uzDesmitniekiem$NDZ_sanemsanas_datums
        ),]
      cat(
        "No vienpadsmitniekiem atvasinātā tabula x11_uzDesmitniekiem pārsūtīta uz processingTens un tad uz tempNDZ, ko būvējam."
      )
      processingTens(x11_uzDesmitniekiem, o)
    } else {
      cat("Tabula x11_uzDesmitniekiem ir tukša.")
    }
    rm(x11_uzDesmitniekiem)
}

processingTens <- function(x, o) {
  x <- x[order(x$period, x$PS_code, x$DN_code, x$NM_code, x$NDZ_sanemsanas_datums), ]

  x10_uzVieniniekiem <- data.frame()
  x10_uzDivniekiem <- data.frame()
  x10_uzSeptini <- data.frame()
  x10_uzAstoniekiem <- data.frame()
  check_rows <- 0
  
  for (r in seq(1, nrow(x), by = 10)) {

    x10 <- x[r:(r+9), ]
    x10 <- x10[order(x10$period, x10$PS_code, x10$DN_code, x10$NM_code, x10$NDZ_sanemsanas_datums), ]
    
    if(sum(x10$start == "1") == 5) {
      if ((x10$start[1] == "1" && x10$end[2] == "2") || ((x10$end[1] == "2" && x10$start[2] == "1") && (x10$NDZ_sanemsanas_datums[1] == x10$NDZ_sanemsanas_datums[2]))) {
        x10_uzDivniekiem <- rbind(x10_uzDivniekiem, x10[1:2, ])
        x10_uzAstoniekiem <- rbind(x10_uzAstoniekiem, x10[3:10, ])
      } else if ((x10$end[1] == "2" && x10$start[2] == "1" && x10$end[3] == "2" && x10$start[4] == "1") && (x10$NDZ_sanemsanas_datums[1] != x10$NDZ_sanemsanas_datums[2])) {
        x10_uzVieniniekiem <- rbind(x10_uzVieniniekiem, x10[1, ])
        x10_uzDivniekiem <- rbind(x10_uzDivniekiem, x10[2:3, ])
        x10_uzSeptini <- rbind(x10_uzSeptini, x10[4:10, ])
      } else if (all(x10$end[c(1, 3, 4, 6, 8)] == "2") && 
                all(x10$start[c(2, 5, 7, 9, 10)] == "1") && 
                all(sapply(seq(4, 9, by = 2), function(i) all(diff(x10$NDZ_sanemsanas_datums[i:i+1]) == 0))) &&
                all(sapply(seq(1, 4, by = 2), function(i) all(diff(x10$NDZ_sanemsanas_datums[i:i+1]) != 0)))) {
        x10_uzVieniniekiem <- rbind(x10_uzVieniniekiem, x10[1, ])
        x10_uzDivniekiem <- rbind(x10_uzDivniekiem, x10[2:3, ])
        x10_uzSeptini <- rbind(x10_uzSeptini, x10[4:10, ])
      } else if (all(x10$end[c(1, 2, 5, 6, 8)] == "2") && 
                 all(x10$start[c(3, 4, 7, 9, 10)] == "1") && 
                 all(sapply(seq(6, 9, by = 2), function(i) all(diff(x10$NDZ_sanemsanas_datums[i:i+1]) == 0))) &&
                 all(sapply(seq(3, 6, by = 2), function(i) all(diff(x10$NDZ_sanemsanas_datums[i:i+1]) != 0))) &&
                 all(diff(x10$NDZ_sanemsanas_datums[2:3]) == 0) &&
                 all(diff(x10$NDZ_sanemsanas_datums[9:10]) != 0)) {
        x10_uzVieniniekiem <- rbind(x10_uzVieniniekiem, x10[1, ])
        x10_uzDivniekiem <- rbind(x10_uzDivniekiem, x10[2:3, ])
        x10_uzSeptini <- rbind(x10_uzSeptini, x10[4:10, ])
      } else if (all(x10$start[c(2, 5, 6, 9, 10)] == "1") && 
                 all(x10$end[c(1, 3, 4, 7, 8)] == "2") && 
                 all(sapply(seq(4, 9, by = 2), function(i) all(diff(x10$NDZ_sanemsanas_datums[i:i+1]) == 0))) &&
                 all(sapply(seq(1, 4, by = 2), function(i) all(diff(x10$NDZ_sanemsanas_datums[i:i+1]) != 0))) &&
                 all(diff(x10$NDZ_sanemsanas_datums[9:10]) != 0)) {
        x10_uzVieniniekiem <- rbind(x10_uzVieniniekiem, x10[1, ])
        x10_uzDivniekiem <- rbind(x10_uzDivniekiem, x10[2:3, ])
        x10_uzSeptini <- rbind(x10_uzSeptini, x10[4:10, ])
      } else {
        stop("processingTens: Desmitnieku tabulas pārdalei trūkst izstrādes koda. Rindas: ", r, " līdz ", r+9, "\n")
      }
    } else if(sum(x10$start == "1") == 4) {
        if (all(x10$end[c(1, 2)] == "2") && all(x10$zinkod[c(1, 2)] == "26") &&
               all(sapply(seq(3, 10, by = 2), function(i) all(diff(x10$NDZ_sanemsanas_datums[i:i+1]) == 0))) &&
               all(sapply(seq(1, 3, by = 2), function(i) all(diff(x10$NDZ_sanemsanas_datums[i:i+1]) != 0)))) {
        x10_uzAstoniekiem <- rbind(x10_uzAstoniekiem, x10[3:10, ])
      } else if (all(x10$start[c(3:4, 7, 9)] == "1") && 
                 all(sapply(seq(1, 10, by = 2), function(i) all(diff(x10$NDZ_sanemsanas_datums[i:i+1]) == 0))) &&
                 all(sapply(seq(2, 9, by = 2), function(i) all(diff(x10$NDZ_sanemsanas_datums[i:i+1]) != 0)))&&
                 x10$PS_code[1] == '___________' & x10$NM_code[1] == '__________') {
        p <- x10[1:6, ]
        p <- p[p$zinkod %in% c("40", "41"), ]
        x10_uzVieniniekiem <- rbind(x10_uzVieniniekiem, p[1, ])
        x10_uzDivniekiem <- rbind(x10_uzDivniekiem, p[2:3, ])
        rm(p)
      } else {
        stop("processingTens: Desmitnieku tabulas pārdalei trūkst izstrādes koda. Rindas: ", r, " līdz ", r+9, "\n")
      }
    } else if (sum(x10$start == "1") == 6) {
      if (all(x10$end[c(2, 4, 7, 9)] == "2") && all(x10$start[c(1, 3, 5, 6, 8, 10)] == "1") &&
          all(sapply(c(1:3, 5:9), function(i) all(diff(x10$NDZ_sanemsanas_datums[i:i+1]) != 0))) && all(diff(x10$NDZ_sanemsanas_datums[4:5]) == 0)) {
        x10_uzVieniniekiem <- rbind(x10_uzVieniniekiem, x10[10, ])
        x10_uzDivniekiem <- rbind(x10_uzDivniekiem, x10[1:2, ])
        x10_uzSeptini <- rbind(x10_uzSeptini, x10[3:9, ])
      } else if (all(x10$start[c(1,2,4,6,8,10)] == "1") && all(diff(x10$NDZ_sanemsanas_datums) != 0)) {
        x10_uzVieniniekiem <- rbind(x10_uzVieniniekiem, x10[10, ])
        x10_uzAstoniekiem <- rbind(x10_uzAstoniekiem, x10[2:9, ])
      } else {
        stop("processingTens: Desmitnieku tabulas pārdalei trūkst izstrādes koda. Rindas: ", r, " līdz ", r+9, "\n")
      }
    } else {
      stop("processingTens: Desmitnieku tabulas pārdalei trūkst izstrādes koda. Rindas: ", r, " līdz ", r+9, "\n")
    }
    check_rows <- check_rows + 10
  }
  
  #2 PĀRBAUDE: Vai rindu skaits no desmitniekiem atvasinātajās tabulās sakrīt ar rindām izejas tabulā x.
  if (nrow(x) == check_rows) {
    cat("PĀRBAUDE IZIETA: Apakštabulu rindu summa sakrīt ar izejošo desmitnieku tabulu.\n")
    rm(x, x10, r, check_rows)
  } else {
    stop("PĀRBAUDE NAV IZIETA: Apakštabulu rindu summa NESAKRĪT ar izejošo desmitnieku tabulu.\n")
  }
  
  #3 Apakštabulu x10_uzVieniniekiem sūta caur processingTwoes().
  if(nrow(x10_uzVieniniekiem) > 0) {
    x10_uzVieniniekiem <- x10_uzVieniniekiem[order(x10_uzVieniniekiem$PS_code, x10_uzVieniniekiem$NM_code, x10_uzVieniniekiem$NDZ_sanemsanas_datums, x10_uzVieniniekiem$start), ]
    cat("No desmitniekiem atvasinātā tabula x10_uzVieniniekiem pārsūtīta uz processingOnes un tad uz tempNDZ, ko būvējam.\n")
    sendTo_tempNDZ(processingOnes(x10_uzVieniniekiem, o))
  } else {
    cat("Tabula x10_uzVieniniekiem ir tukša.\n")
  }
  rm(x10_uzVieniniekiem) 
  
  #4 Apakštabulu x10_uzDivniekiem sūta caur processingTwoes().
  if(nrow(x10_uzDivniekiem) > 0) {
    x10_uzDivniekiem <- x10_uzDivniekiem[order(x10_uzDivniekiem$PS_code, x10_uzDivniekiem$NM_code, x10_uzDivniekiem$NDZ_sanemsanas_datums, x10_uzDivniekiem$start), ]
    cat("No desmitniekiem atvasinātā tabula x10_uzDivniekiem pārsūtīta uz processingTwoes un caur to uz tempNDZ, ko būvējam.\n")
    processingTwoes(x10_uzDivniekiem, o)
  } else {
    cat("Tabula x10_uzDivniekiem ir tukša.\n")
  }
  rm(x10_uzDivniekiem)
  
  #5 Apakštabulu x10_uzSeptini sūta caur processingSeven().
  if(nrow(x10_uzSeptini) > 0) {
    x10_uzSeptini <- x10_uzSeptini[order(x10_uzSeptini$PS_code, x10_uzSeptini$NM_code, x10_uzSeptini$NDZ_sanemsanas_datums, x10_uzSeptini$start), ]
    cat("No desmitniekiem atvasinātā tabula x10_uzSeptini pārsūtīta uz processingSeven un caur to uz tempNDZ, ko būvējam.\n")
    processingSeven(x10_uzSeptini, o)
  } else {
    cat("Tabula x10_uzSeptini ir tukša.\n")
  }
  rm(x10_uzSeptini)
  
  #6 Apakštabulu x10_uzAstoniekiem sūta caur processingEights().
  if(nrow(x10_uzAstoniekiem) > 0) {
    x10_uzAstoniekiem <- x10_uzAstoniekiem[order(x10_uzAstoniekiem$PS_code, x10_uzAstoniekiem$NM_code, x10_uzAstoniekiem$NDZ_sanemsanas_datums), ]
    cat("No desmitniekiem atvasinātā tabula x10_uzAstoniekiem pārsūtīta uz processingEights un tad uz tempNDZ, ko būvējam.\n")
    processingEights(x10_uzAstoniekiem, o)
  } else {
    cat("Tabula x10_uzAstoniekiem ir tukša.\n")
  }
  rm(x10_uzAstoniekiem) 
}

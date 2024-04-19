processingTwelwe <- function(x, o) {

  x <- x[order(x$PS_code, x$DN_code, x$NM_code, x$NDZ_sanemsanas_datums, x$start), ]
  x12_uzVieniniekiem <- data.frame()
  x12_uzDivniekiem <- data.frame()
  x12_uzDesmitniekiem <- data.frame()
  x12_uzVienpadsmit <- data.frame()

  for (r in seq(1, nrow(x), by = 12)) {
    #TESTĒŠANAI r <- sample(1: nrow(x), size = 1, replace = FALSE)
    x12 <- x[r:(r+11),]
    x12 <- x12[order(x12$PS_code, x12$DN_code, x12$NM_code, x12$NDZ_sanemsanas_datums), ]
    # TESTĒŠANAI: x[x$PS_code == x$PS_code[r], ]
    
    if (sum(x12$end == "2") == 6) {
      if ((x12$end[1] == "2" && x12$start[2] == "1" && x12$end[3] == "2") && (x12$NDZ_sanemsanas_datums[1] != x12$NDZ_sanemsanas_datums[2])) {
        x12_uzVieniniekiem <- rbind(x12_uzVieniniekiem, x12[1, ])
        x12_uzVienpadsmit <- rbind(x12_uzVienpadsmit, x12[2:12, ])
      } else if ((x12$start[1] == "1" && x12$end[2] == "2" && x12$start[3] == "1") && (x12$NDZ_sanemsanas_datums[2] != x12$NDZ_sanemsanas_datums[3])) {
        x12_uzDivniekiem <- rbind(x12_uzDivniekiem, x12[1:2, ])
        x12_uzDesmitniekiem <- rbind(x12_uzDesmitniekiem, x12[-(1:2), ])
      } else if (x12$end[1] == "2" && x12$start[2] == "1" && x12$NDZ_sanemsanas_datums[1] == x12$NDZ_sanemsanas_datums[2]) {
        x12_uzDivniekiem <- rbind(x12_uzDivniekiem, x12[1:2, ])
        x12_uzDesmitniekiem <- rbind(x12_uzDesmitniekiem, x12[-(1:2), ])
      } else if (x12$start[1] == "1" && x12$end[2] == "2" && 
                 x12$NDZ_sanemsanas_datums[1] <= x12$NDZ_sanemsanas_datums[2]) {
        x12_uzDivniekiem <- rbind(x12_uzDivniekiem, x12[1:2, ])
        x12_uzDesmitniekiem <- rbind(x12_uzDesmitniekiem, x12[-(1:2), ])
      } else if (all(x12$end[c(1,2, 4)] == "2") && all(x12$start[c(3, 5)] == "1") && 
                 x12$NDZ_sanemsanas_datums[1] != x12$NDZ_sanemsanas_datums[2] &&
                 all(sapply(seq(2, 5, by = 2), function(i) all(diff(x12$NDZ_sanemsanas_datums[i:i+1]) == 0)))) {
        x12_uzVieniniekiem <- rbind(x12_uzVieniniekiem, x12[1, ])
        x12_uzVienpadsmit <- rbind(x12_uzVienpadsmit, x12[-1, ])
      } else {
        stop(cat("12-nieku tabulas pārdalei trūkst izstrādes koda. Rindas:",r, " līdz ", r + 11))
      }
    } else {
      stop(cat("12-nieku tabulas pārdalei trūkst izstrādes koda. Rindas:",r, "līdz", r + 11))
    }
  }
  
  #2 PĀRBAUDE: Vai rindu skaits no 12-niekiem atvasinātajās tabulās sakrīt ar rindām izejas tabulā x.
  if ((nrow(x12_uzVieniniekiem) + nrow(x12_uzDivniekiem) + nrow(x12_uzDesmitniekiem) + nrow(x12_uzVienpadsmit)) == nrow(x)) {
    cat("PĀRBAUDE IZIETA: Apakštabulu rindu summa sakrīt ar izejošo 12-nieku tabulu.\n")
    rm(x, x12, r)
  } else {
    stop("PĀRBAUDE NAV IZIETA: Apakštabulu rindu summa NESAKRĪT ar izejošo 12-nieku tabulu.")
  }
  
  #3 Apakštabulu x12_uzVieniniekiem sūta caur processingOnes().
  if (nrow(x12_uzVieniniekiem) > 0) {
    x12_uzVieniniekiem <- x12_uzVieniniekiem[order(x12_uzVieniniekiem$PS_code, x12_uzVieniniekiem$NM_code, x12_uzVieniniekiem$NDZ_sanemsanas_datums, x12_uzVieniniekiem$start),]
    cat("No 12-niekiem atvasinātā tabula x12_uzVieniniekiem pārsūtīta uz processingOnes() un tad uz tempNDZ, ko būvējam.\n")
    sendTo_tempNDZ(processingOnes(x12_uzVieniniekiem, o))
  } else {
    cat("Tabula x12_uzVieniniekiem ir tukša.\n")
  }
  
  rm(x12_uzVieniniekiem)
  
  #4 Apakštabulu x12_uzDivniekiem sūta caur processingTwoes().
  if (nrow(x12_uzDivniekiem) > 0) {
    x12_uzDivniekiem <- x12_uzDivniekiem[order(x12_uzDivniekiem$PS_code, x12_uzDivniekiem$NM_code, x12_uzDivniekiem$NDZ_sanemsanas_datums, x12_uzDivniekiem$start),]
    cat("No 12-niekiem atvasinātā tabula x12_uzDivniekiem pārsūtīta uz processingTwoes() un caur to uz tempNDZ, ko būvējam.\n")
    processingTwoes(x12_uzDivniekiem, o)
  } else {
    cat("Tabula x12_uzDivniekiem ir tukša.\n")
  }
  
  rm(x12_uzDivniekiem) 
  
  #5 Apakštabulu x12_uzDesmitniekiem sūta caur processingTens().
  if (nrow(x12_uzDesmitniekiem) > 0) {
    x12_uzDesmitniekiem <-
      x12_uzDesmitniekiem[order(
        x12_uzDesmitniekiem$PS_code,
        x12_uzDesmitniekiem$NM_code,
        x12_uzDesmitniekiem$NDZ_sanemsanas_datums, x12_uzDesmitniekiem$start
      ),]
    cat("No 12-niekiem atvasinātā tabula x12_uzDesmitniekiem pārsūtīta uz processingTens un tad uz tempNDZ, ko būvējam.\n")
    processingTens(x12_uzDesmitniekiem, o)
  } else {
    cat("Tabula x12_uzDesmitniekiem ir tukša.\n")
  }
  rm(x12_uzDesmitniekiem)
  
  #6 Apakštabulu x12_uzVienpadsmit sūta caur processingEleven().
  if (nrow(x12_uzVienpadsmit) > 0) {
    x12_uzVienpadsmit <-
      x12_uzVienpadsmit[order(
        x12_uzVienpadsmit$PS_code,
        x12_uzVienpadsmit$NM_code,
        x12_uzVienpadsmit$NDZ_sanemsanas_datums, x12_uzVienpadsmit$start
      ),]
    cat(
      "No 12-niekiem atvasinātā tabula x12_uzVienpadsmit pārsūtīta uz processingEleven un tad uz tempNDZ, ko būvējam.\n")
    processingEleven(x12_uzVienpadsmit, o)
  } else {
    cat("Tabula x12_uzVienpadsmit ir tukša.\n")
  }
  rm(x12_uzVienpadsmit)
}

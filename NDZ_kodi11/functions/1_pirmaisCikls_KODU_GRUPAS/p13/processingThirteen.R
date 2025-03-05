processingThirteen <- function(x, o, kods) {
  cat("-------------SĀK 13-nieku APSTRĀDI.")
  x <- x %>% arrange(PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
  
  x13_uzVieniniekiem <- data.frame(); x13_uzDivi <- data.frame(); x13_uzTrijniekiem <- data.frame(); x13_uzCetriniekiem <- data.frame()
  x13_uzAstoniekiem <- data.frame(); x13_uzDesmitniekiem <- data.frame(); x13_uzVienpadsmit <- data.frame()
  check_rows <- 0
  
  result <- function(y) {
    x13_uzVieniniekiem <<- rbind(x13_uzVieniniekiem, y$x13_uzVieniniekiem)
    x13_uzDivi <<- rbind(x13_uzDivi, y$x13_uzDivi)
    x13_uzTrijniekiem <<- rbind(x13_uzTrijniekiem, y$x13_uzTrijniekiem)
    x13_uzCetriniekiem <<- rbind(x13_uzCetriniekiem, y$x13_uzCetriniekiem)
    x13_uzAstoniekiem <<- rbind(x13_uzAstoniekiem, y$x13_uzAstoniekiem)
    x13_uzDesmitniekiem <<- rbind(x13_uzDesmitniekiem, y$x13_uzDesmitniekiem)
    x13_uzVienpadsmit <<- rbind(x13_uzVienpadsmit, y$x13_uzVienpadsmit)
    rm(y)}
  
  
  for (r in seq(1, nrow(x), by = 13)) {
    
    x13 <- x[r:(r+12),] %>% arrange(PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
    
    if (sum(x13$sak_beidz == "2") == 7) {
          result(processingThirteen_b7(a, o, kods))
    } else if (sum(x13$sak_beidz == "1") == 7) {
          result(processingThirteen_s7(a, o, kods))
    } else {stop("13-nieku tabulas pārdalei trūkst izstrādes koda. Rindas:",r, "līdz", r + 12, "\n")}
          
    check_rows <- check_rows + 13
  }
  
  #PĀRBAUDE: Vai rindu skaits no 13-niekiem atvasinātajās tabulās sakrīt ar rindām izejas tabulā x.
  if (check_rows == nrow(x)) {
    cat("PĀRBAUDE IZIETA: Apakštabulu rindu summa sakrīt ar izejošo 13-nieku tabulu.\n")
    rm(x, x13, r, check_rows)
  } else {stop("ERROR: Apakštabulu rindu summa NESAKRĪT ar izejošo 13-nieku tabulu.\n")}
  
  #1 Apakštabulu x13_uzVieniniekiem sūta caur processingOnes().
  if (nrow(x13_uzVieniniekiem) > 0) {
    x13_uzVieniniekiem %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums, sak_beidz) %>% processingOnes(o) %>% sendTo_tempNDZ(o)
  } else {cat("Tabula x13_uzVieniniekiem ir tukša.\n")}
  rm(x13_uzVieniniekiem)

  #2 Apakštabulu x13_uzDivi sūta caur processingTwoes().
  if (nrow(x13_uzDivi) > 0) {
    x13_uzDivi %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingTwoes(o, kods)
  } else {cat("Tabula x13_uzDivi ir tukša.\n")}
  rm(x13_uzDivi)
  
  #3 Apakštabulu x13_uzTrijniekiem sūta caur processingThrees().
  if (nrow(x13_uzTrijniekiem) > 0) {
    x13_uzTrijniekiem %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingThrees(o, kods)
  } else {cat("Tabula x13_uzTrijniekiem ir tukša.\n")}
  rm(x13_uzTrijniekiem)
  
  #4 Apakštabulu x13_uzCetriniekiem sūta caur processingFours().
  if (nrow(x13_uzCetriniekiem) > 0) {
    x13_uzCetriniekiem %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingFours(o, kods)
  } else {cat("Tabula x13_uzCetriniekiem ir tukša.\n")}
  rm(x13_uzCetriniekiem)
  
  #5 Apakštabulu x13_uzAstoniekiem sūta caur processingEights().
  if (nrow(x13_uzAstoniekiem) > 0) {
    x13_uzAstoniekiem %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingEights(o, kods)
  } else {cat("Tabula x13_uzAstoniekiem ir tukša.\n")}
  rm(x13_uzAstoniekiem)
  
  #6 Apakštabulu x13_uzDesmitniekiem sūta caur processingTens().
  if (nrow(x13_uzDesmitniekiem) > 0) {
    x13_uzDesmitniekiem %>% arrange (PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingTens(o, kods)
  } else {cat("Tabula x13_uzDesmitniekiem ir tukša.\n")}
  rm(x13_uzDesmitniekiem)
  
  #6 Apakštabulu x13_uzVienpadsmit sūta caur processingEleven().
  if (nrow(x13_uzVienpadsmit) > 0) {
    x13_uzVienpadsmit %>% arrange (PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingEleven(o, kods)
  } else {cat("Tabula x13_uzVienpadsmit ir tukša.\n")}
  rm(x13_uzVienpadsmit)
}

processingTwelve <- function(x, o, kods) {
  cat("-------------SĀK 12-nieku APSTRĀDI. \n")
  x <- arrange(x, PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
  x12_uzVieniniekiem <- data.frame(); x12_uzDivniekiem <- data.frame(); x12_uzSeptini <- data.frame(); x12_uzAstoni <- data.frame(); x12_uzDevini <- data.frame(); x12_uzDesmitniekiem <- data.frame(); x12_uzVienpadsmit <- data.frame()
  check_rows <- 0
  
  result <- function(y) {
    x12_uzVieniniekiem <<- rbind(x12_uzVieniniekiem, y$x12_uzVieniniekiem)
    x12_uzDivniekiem <<- rbind(x12_uzDivniekiem, y$x12_uzDivniekiem)
    x12_uzSeptini <<- rbind(x12_uzSeptini, y$x12_uzSeptini)
    x12_uzAstoni <<- rbind(x12_uzAstoni, y$x12_uzAstoni)
    x12_uzDesmitniekiem <<- rbind(x12_uzDesmitniekiem, y$x12_uzDesmitniekiem)
    x12_uzVienpadsmit <<- rbind(x12_uzVienpadsmit, y$x12_uzVienpadsmit)
    rm(y)
  }
  
  for (r in seq(1, nrow(x), by = 12)) {
    x12 <- x[r:(r+11),] %>% arrange(PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
    
    if (sum(x12$sak_beidz == "1") == 5) {
      result(processingTwelve_s5(x12, o, kods))
    } else if (sum(x12$sak_beidz == "2") == 6) {
        result(processingTwelve_b6(x12, o, kods))
    } else if (sum(x12$sak_beidz == "1") == 8) {
        result(processingTwelve_s8(x12, o, kods))
    } else {stop("12-nieku tabulas pārdalei trūkst izstrādes koda. Rindas:",r, " līdz ", r + 11)}
    check_rows <- check_rows + 12
  }
  
#PĀRBAUDE: Vai rindu skaits no 12-niekiem atvasinātajās tabulās sakrīt ar rindām izejas tabulā x.
  if (check_rows == nrow(x)) {
    cat("PĀRBAUDE IZIETA: Apakštabulu rindu summa sakrīt ar izejošo 12-nieku tabulu.\n"); rm(x, x12, r, check_rows)
  } else {stop("PĀRBAUDE NAV IZIETA: Apakštabulu rindu summa NESAKRĪT ar izejošo 12-nieku tabulu.")}
  
#1 Apakštabulu x12_uzVieniniekiem sūta caur processingOnes().
if (nrow(x12_uzVieniniekiem) > 0) {
    x12_uzVieniniekiem %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingOnes(o) %>% sendTo_tempNDZ(o)
} else {cat("Tabula x12_uzVieniniekiem ir tukša.\n")}
rm(x12_uzVieniniekiem)
  
#2 Apakštabulu x12_uzDivniekiem sūta caur processingTwoes().
if (nrow(x12_uzDivniekiem) > 0) {
    x12_uzDivniekiem %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingTwoes(o, kods)
} else {cat("Tabula x12_uzDivniekiem ir tukša.\n")}
rm(x12_uzDivniekiem) 
  
#3 Apakštabulu x12_uzSeptini sūta caur processingSeven().
if (nrow(x12_uzSeptini) > 0) {
    x12_uzSeptini %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingSeven(o, kods)
} else {cat("Tabula x12_uzSeptini ir tukša.\n")}
rm(x12_uzSeptini)

#4 Apakštabulu x12_uzSeptini sūta caur processingSeven().
if (nrow(x12_uzAstoni) > 0) {
  x12_uzAstoni %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingEights(o, kods)
} else {cat("Tabula x12_uzAstoni ir tukša.\n")}
rm(x12_uzAstoni)

#5 Apakštabulu x12_uzDevini sūta caur processingNines().
if (nrow(x12_uzDevini) > 0) {
  x12_uzDevini %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingNines(o, kods)
} else {cat("Tabula x12_uzDevini ir tukša.\n")}
rm(x12_uzDevini)

#6 Apakštabulu x12_uzDesmitniekiem sūta caur processingTens().
if (nrow(x12_uzDesmitniekiem) > 0) {
    x12_uzDesmitniekiem %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingTens(o, kods)
} else {cat("Tabula x12_uzDesmitniekiem ir tukša.\n")}
rm(x12_uzDesmitniekiem)
  
#7 Apakštabulu x12_uzVienpadsmit sūta caur processingEleven().
if (nrow(x12_uzVienpadsmit) > 0) {
    x12_uzVienpadsmit %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingEleven(o, kods)
} else {cat("Tabula x12_uzVienpadsmit ir tukša.\n")}
rm(x12_uzVienpadsmit)
}

processingEleven <- function(x, o, kods) {
  cat("-------------SĀK 11-nieku APSTRĀDI. \n")
  x <- arrange(x, PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)

  x11_uzVieniniekiem <- data.frame(); x11_2 <- data.frame(); x11_uzDevini <- data.frame(); x11_uzDesmitniekiem <- data.frame()
  check_rows <- 0
  
  result <- function(y) {
     x11_uzVieniniekiem <<- rbind(x11_uzVieniniekiem, y$x11_uzVieniniekiem)
     x11_2 <<- rbind(x11_2, y$x11_2)
     x11_uzDevini <<- rbind(x11_uzDevini, y$x11_uzDevini)
     x11_uzDesmitniekiem <<- rbind(x11_uzDesmitniekiem, y$x11_uzDesmitniekiem)
    rm(y)}
  
  for (r in seq(1, nrow(x), by = 11)) {
    x11 <- x[r:(r+10),] %>% arrange(PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
      
    if (sum(x11$sak_beidz == "1") == 6) {
          result(processingEleven_s6(x11, o, kods))
    } else if (sum(x11$sak_beidz == "2") == 6) {
        if (all(x11$sak_beidz[1:2] == c("2", "1")) && diff(x11$NDZ_sanemsanas_datums[1:2]) != 0) {
          x11_uzVieniniekiem <- rbind(x11_uzVieniniekiem, x11[1,])
          x11_uzDesmitniekiem <- rbind(x11_uzDesmitniekiem, x11[2:11,])
        } else {stop("processingEleven: Vienpadsmitnieku tabulas pārdalei trūkst izstrādes koda. Rindas: ", r, " līdz ", r + 10, "\n")}
      } else {stop("processingEleven: Vienpadsmitnieku tabulas pārdalei trūkst izstrādes koda. Rindas: ", r, " līdz ", r + 10, "\n")}
      check_rows <- check_rows + 11
    }
    
#PĀRBAUDE: Vai rindu skaits no vienpadsmitniekiem atvasinātajās tabulās sakrīt ar rindām izejas tabulā x.
if (check_rows == nrow(x)) {
      cat("PĀRBAUDE IZIETA: Apakštabulu rindu summa sakrīt ar izejošo vienpadsmitnieku tabulu.\n"); rm(x, x11, r, check_rows)
} else {stop("processingEleven: PĀRBAUDE NAV IZIETA. Apakštabulu rindu summa NESAKRĪT ar izejošo vienpadsmitnieku tabulu.")}

#1 Apakštabulu x11_uzVieniniekiem sūta caur funkciju processingOnes().
if (nrow(x11_uzVieniniekiem) > 0) {
      x11_uzVieniniekiem %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingOnes(o) %>% sendTo_tempNDZ(o)
} else {cat("Tabula x11_uzVieniniekiem ir tukša.\n")}
rm(x11_uzVieniniekiem)

#2 Apakštabulu x11_2 sūta caur funkciju processingTwoes().
if (nrow(x11_2) > 0) {
  x11_2 %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingTwoes(o, kods)
} else {cat("Tabula x11_2 ir tukša.\n")}
rm(x11_2)

#3 Apakštabulu x11_uzDeviņi sūta caur processingNines().
if (nrow(x11_uzDevini) > 0) {
      x11_uzDevini %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingNines(o, kods)
} else {cat("Tabula x11_uzDevini ir tukša.\n")}
rm(x11_uzDevini)
    
#4 Apakštabulu x11_uzDesmitniekiem sūta caur processingTens().
if (nrow(x11_uzDesmitniekiem) > 0) {
      x11_uzDesmitniekiem %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingTens(o, kods)
} else {cat("Tabula x11_uzDesmitniekiem ir tukša.\n")}
rm(x11_uzDesmitniekiem)
}

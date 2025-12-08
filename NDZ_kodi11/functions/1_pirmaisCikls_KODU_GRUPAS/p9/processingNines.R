processingNines <- function(x, o, kods) {
  x <- arrange(x, PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
  x9_uzVieniniekiem <- data.frame(); x9_uzDivi <- data.frame();  x9_uzPieci <- data.frame(); x9_uzSesi <- data.frame(); x9_uzSeptini <- data.frame(); x9_uzAstoniekiem <- data.frame()
  check_rows <- 0

  result <- function(res) {
    x9_uzVieniniekiem <<- rbind(x9_uzVieniniekiem, res$x9_uzVieniniekiem)
    x9_uzDivi <<- rbind(x9_uzDivi, res$x9_uzDivi)
    x9_uzPieci <<- rbind(x9_uzPieci, res$x9_uzPieci)
    x9_uzSesi <<- rbind(x9_uzSesi, res$x9_uzSesi)
    x9_uzSeptini <<- rbind(x9_uzSeptini, res$x9_uzSeptini)
    x9_uzAstoniekiem <<- rbind(x9_uzAstoniekiem, res$x9_uzAstoniekiem)
    rm(res)}
  
  for(r in seq(1, nrow(x), by = 9)) {

    x9 <- x[r:(r+8),] %>% arrange(PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
    
    if (all(x9$sak_beidz[1:3] == c("1", "1", "2")) && x9$sak_beidz[4] != "2") {
               result(processingNines_112X2(x9, o, kods))
    } else if (all(x9$sak_beidz[1:4] == c("1", "2", "1", "1"))) {
               result(processingNines_1211(x9, o, kods))
    } else if (all(x9$sak_beidz[1:4] == c("2", "2", "1", "1"))) {
               result(processingNines_2211(x9, o, kods))
    } else if (all(x9$sak_beidz[1:4] == c("1", "2", "1", "2"))) {
               result(processingNines_1212(x9, o, kods))
    } else if (all(x9$sak_beidz[1:4] == c("2", "2", "1", "2"))) {
               result(processingNines_2212(x9, o, kods))
    } else if (all(x9$sak_beidz[1:4] == c("2", "1", "1", "2"))) {
               result(processingNines_2112(x9, o, kods))
    } else if (all(x9$sak_beidz[1:4] == c("1", "2", "2", "1"))) {
               result(processingNines_1221(x9, o, kods))
    } else if (all(x9$sak_beidz[1:4] == c("2", "1", "2", "1"))) {
               result(processingNines_2121(x9, o, kods))
    } else if (all(x9$sak_beidz[1:4] == c("2", "1", "2", "2"))) {
               result(processingNines_2122(x9, o, kods))
    } else {stop("processingNines() iztrūkst apstrādes kods: rindas ", r, ":", r+8, "!\n")}

        check_rows = check_rows + 9
  }
  
#PĀRBAUDE: Vai rindu skaits no deviņniekiem atvasinātajās tabulās sakrīt ar rindām izejas tabulā x.
if (nrow(x) == check_rows) {
  cat("PĀRBAUDE IZIETA: Apakštabulu rindu summa sakrīt ar izejošo devītnieku tabulu.\n")
  rm(x, x9)
} else {stop("PĀRBAUDE NAV IZIETA: Apakštabulu rindu summa NESAKRĪT ar izejošo devītnieku tabulu.\n")}
  
#1 Apakštabulu x9_uzVieniniekiem sūta caur processingOnes().
if(nrow(x9_uzVieniniekiem) > 0) {
    x9_uzVieniniekiem %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingOnes(o) %>% sendTo_tempNDZ(o)
} else {cat("Tabula x9_uzVieniniekiem ir tukša.\n")}
rm(x9_uzVieniniekiem)

#2 Apakštabulu x9_uzDivi sūta caur processingTwoes().
if(nrow(x9_uzDivi) > 0) {
  x9_uzDivi %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingTwoes(o, kods)
} else {cat("Tabula x9_uzDivi ir tukša.\n")}
rm(x9_uzDivi) 

#3 Apakštabulu x9_uzPieci sūta caur processingFives().
if(nrow(x9_uzPieci) > 0) {
  x9_uzPieci %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingFives(o, kods)
} else {cat("Tabula x9_uzPieci ir tukša.\n")}
rm(x9_uzPieci) 

#4 Apakštabulu x9_uzSesi sūta caur processingSixes().
if(nrow(x9_uzSesi) > 0) {
  x9_uzSesi %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingSixes(o, kods)
} else {cat("Tabula x9_uzSesi ir tukša.\n")}
rm(x9_uzSesi) 

#5 Apakštabulu x9_uzSeptini sūta caur processingSeven().
if(nrow(x9_uzSeptini) > 0) {
  x9_uzSeptini %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingSeven(o, kods)
} else {cat("Tabula x9_uzSeptini ir tukša.\n")}
rm(x9_uzSeptini) 

#6 Apakštabulu x9_uzAstoniekiem sūta caur processingEights().
if(nrow(x9_uzAstoniekiem) > 0) {
  x9_uzAstoniekiem %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingEights(o, kods)
} else {cat("Tabula x9_uzAstoniekiem ir tukša.\n")}
rm(x9_uzAstoniekiem) 
}


processingTens <- function(x, o, kods) {
  cat("-------------SĀK 10-nieku APSTRĀDI.")
  x <- x %>% arrange(PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)

  x10_uzVieniniekiem <- data.frame(); x10_uzDivniekiem <- data.frame(); x10_uzPieci <- data.frame(); x10_uzSeptini <- data.frame(); x10_uzAstoniekiem <- data.frame()
  check_rows <- 0

  result <- function(y) {
    x10_uzVieniniekiem <<- rbind(x10_uzVieniniekiem, y$x10_uzVieniniekiem)
    x10_uzDivniekiem <<- rbind(x10_uzDivniekiem, y$x10_uzDivniekiem)
    x10_uzPieci <<- rbind(x10_uzPieci, y$x10_uzPieci)
    x10_uzSeptini <<- rbind(x10_uzSeptini, y$x10_uzSeptini)
    x10_uzAstoniekiem <<- rbind(x10_uzAstoniekiem, y$x10_uzAstoniekiem)
    rm(y)
  }
  
  for (r in seq(1, nrow(x), by = 10)) {
    x10 <- x[r:(r+9),] %>% arrange(PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
  
    if (sum(x10$sak_beidz == "1") == 5) {
            result(processingTens_s5(x10, o, kods))
    } else if (sum(x10$sak_beidz == "1") == 4) {
              if (all(sapply(seq(1, 10, by = 2), function(i) diff(x10$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
                  all(sapply(seq(2, 8, by = 2), function(i) diff(x10$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
                if (all(x10$sak_beidz[c(1, 2)] == "2") && all(x10$zinkod[c(1, 2)] == "26")) {
                  x10_uzAstoniekiem <- rbind(x10_uzAstoniekiem, x10[3:10, ])
                } else if (all(x10$sak_beidz[c(1,2,5,6,9,10)] == "2") && all(x10$sak_beidz[c(3,4,7,8)] == "1") && "91" %in% x10$zinkod) {
                  x10_uzVieniniekiem <- rbind(x10_uzVieniniekiem, x10[2,])
                  x10_uzDivniekiem <- rbind(x10_uzDivniekiem, x10[c(3,5,7,9), ])
                } else {stop("processingTens: Desmitnieku tabulas pārdalei trūkst izstrādes koda. \n")}
              } else if (all(x10$sak_beidz[c(3:4, 7, 9)] == "1") && 
                             all(sapply(seq(1, 10, by = 2), function(i) all(diff(x10$NDZ_sanemsanas_datums[i:(i+1)]) == 0))) &&
                             all(sapply(seq(2, 9, by = 2), function(i) all(diff(x10$NDZ_sanemsanas_datums[i:(i+1)]) != 0)))&&
                             x10$PS_code[1] == '__________' & x10$NM_code[1] == '__________') {
                    p <- x10[1:6, ]
                    p <- p[p$zinkod %in% c("40", "41"), ]
                    x10_uzVieniniekiem <- rbind(x10_uzVieniniekiem, p[1, ])
                    x10_uzDivniekiem <- rbind(x10_uzDivniekiem, p[2:3, ])
                    rm(p)
              } else if (all(diff(x10$NDZ_sanemsanas_datums) != 0)) {
                  if (all(x10$sak_beidz[c(2,5,8,10)] == "1")) {
                    if (all(x10$sak_beidz[c(2,5,8,10)] == "1")) {
                      if (x10$period[1] == "_____" && x10$PS_code[1] == "__________" && x10$NM_code[1] == "__________") {
                        x10_uzVieniniekiem <- rbind(x10_uzVieniniekiem, x10[c(1,10), ])
                        x10_uzDivniekiem <- rbind(x10_uzDivniekiem, x10[c(2,4,5,7,8,9), ])
                      } else {stop("processingTens: Trūkst izstrādes koda. \n")}}
                    } else {stop("processingTens: Trūkst izstrādes koda. \n")}}
    } else if (sum(x10$sak_beidz == "1") == 6) {
          result(processingTens_s6(x10, o, kods))
    } else if (sum(x10$sak_beidz == "1") == 7) {
          result(processingTens_s7(x10, o, kods))
    } else {stop("processingTens: Desmitnieku tabulas pārdalei trūkst izstrādes koda. Rindas: ", r, " līdz ", r+9, "\n")}
    
    check_rows <- check_rows + 10
  }
  
#PĀRBAUDE: Vai rindu skaits no desmitniekiem atvasinātajās tabulās sakrīt ar rindām sākotnējā tabulā x.
if (nrow(x) == check_rows) {
    cat("PĀRBAUDE IZIETA: Apakštabulu rindu summa sakrīt ar izejošo desmitnieku tabulu.\n"); rm(x, x10, r, check_rows)
} else {stop("PĀRBAUDE NAV IZIETA: Apakštabulu rindu summa NESAKRĪT ar izejošo desmitnieku tabulu.\n")}
  
#1 Apakštabulu x10_uzVieniniekiem sūta caur processingOnes().
if(nrow(x10_uzVieniniekiem) > 0) {
  x10_uzVieniniekiem %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingOnes(o) %>% sendTo_tempNDZ(o)
} else {cat("Tabula x10_uzVieniniekiem ir tukša.\n")}
rm(x10_uzVieniniekiem) 
  
#2 Apakštabulu x10_uzDivniekiem sūta caur processingTwoes().
if(nrow(x10_uzDivniekiem) > 0) {
  x10_uzDivniekiem %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingTwoes(o, kods)
} else {cat("Tabula x10_uzDivniekiem ir tukša.\n")}
rm(x10_uzDivniekiem)
  
#3 Apakštabulu x10_uzPieci sūta caur processingFives().
if(nrow(x10_uzPieci) > 0) {
  x10_uzPieci %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingFives(o, kods)
} else {cat("Tabula x10_uzPieci ir tukša.\n")}
rm(x10_uzPieci)

#4 Apakštabulu x10_uzSeptini sūta caur processingSeven().
if(nrow(x10_uzSeptini) > 0) {
  x10_uzSeptini %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingSeven(o, kods)
} else {cat("Tabula x10_uzSeptini ir tukša.\n")}
rm(x10_uzSeptini)
  
#5 Apakštabulu x10_uzAstoniekiem sūta caur processingEights().
if(nrow(x10_uzAstoniekiem) > 0) {
  x10_uzAstoniekiem %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingEights(o, kods)
} else {cat("Tabula x10_uzAstoniekiem ir tukša.\n")}
rm(x10_uzAstoniekiem) 
}

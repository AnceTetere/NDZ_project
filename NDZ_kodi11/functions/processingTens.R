processingTens <- function(x, o) {
  cat("-------------SĀK 10-nieku APSTRĀDI.")
  x <- arrange(x, PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)

  x10_uzVieniniekiem <- data.frame(); x10_uzDivniekiem <- data.frame(); x10_uzSeptini <- data.frame(); x10_uzAstoniekiem <- data.frame()
  check_rows <- 0
  
  for (r in seq(1, nrow(x), by = 10)) {

    x10 <- x[r:(r+9),] %>% arrange(PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
    
    if(sum(x10$sak_beidz == "1") == 5) {
      if (all(x10$sak_beidz[1:2] == c("1","2")) || (all(x10$sak_beidz[1:2] == c("2","1")) && diff(x10$NDZ_sanemsanas_datums[1:2]) == 0)) {
        x10_uzDivniekiem <- rbind(x10_uzDivniekiem, x10[1:2, ])
        x10_uzAstoniekiem <- rbind(x10_uzAstoniekiem, x10[3:10, ])
      } else if (all(x10$sak_beidz[1:4] == c("2", "1", "2", "1")) && 
                 diff(x10$NDZ_sanemsanas_datums[1:2]) != 0) {
        x10_uzVieniniekiem <- rbind(x10_uzVieniniekiem, x10[1, ])
        x10_uzDivniekiem <- rbind(x10_uzDivniekiem, x10[2:3, ])
        x10_uzSeptini <- rbind(x10_uzSeptini, x10[4:10, ])
      } else if (all(x10$sak_beidz[c(1, 3, 4, 6, 8)] == "2") && 
                 all(x10$sak_beidz[c(2, 5, 7, 9, 10)] == "1") && 
                 all(sapply(seq(4, 9, by = 2), function(i) all(diff(x10$NDZ_sanemsanas_datums[i:i+1]) == 0))) &&
                 all(sapply(seq(1, 4, by = 2), function(i) all(diff(x10$NDZ_sanemsanas_datums[i:i+1]) != 0)))) {
        x10_uzVieniniekiem <- rbind(x10_uzVieniniekiem, x10[1, ])
        x10_uzDivniekiem <- rbind(x10_uzDivniekiem, x10[2:3, ])
        x10_uzSeptini <- rbind(x10_uzSeptini, x10[4:10, ])
      } else if (all(x10$sak_beidz[c(1, 2, 5, 6, 8)] == "2") && 
                 all(x10$sak_beidz[c(3, 4, 7, 9, 10)] == "1") && 
                 all(sapply(seq(6, 9, by = 2), function(i) all(diff(x10$NDZ_sanemsanas_datums[i:i+1]) == 0))) &&
                 all(sapply(seq(3, 6, by = 2), function(i) all(diff(x10$NDZ_sanemsanas_datums[i:i+1]) != 0))) &&
                 all(diff(x10$NDZ_sanemsanas_datums[2:3]) == 0) &&
                 all(diff(x10$NDZ_sanemsanas_datums[9:10]) != 0)) {
        x10_uzVieniniekiem <- rbind(x10_uzVieniniekiem, x10[1, ])
        x10_uzDivniekiem <- rbind(x10_uzDivniekiem, x10[2:3, ])
        x10_uzSeptini <- rbind(x10_uzSeptini, x10[4:10, ])
      } else if (all(x10$sak_beidz[c(2, 5, 6, 9, 10)] == "1") && 
                 all(x10$sak_beidz[c(1, 3, 4, 7, 8)] == "2") && 
                 all(sapply(seq(4, 9, by = 2), function(i) all(diff(x10$NDZ_sanemsanas_datums[i:i+1]) == 0))) &&
                 all(sapply(seq(1, 4, by = 2), function(i) all(diff(x10$NDZ_sanemsanas_datums[i:i+1]) != 0))) &&
                 all(diff(x10$NDZ_sanemsanas_datums[9:10]) != 0)) {
        x10_uzVieniniekiem <- rbind(x10_uzVieniniekiem, x10[1, ])
        x10_uzDivniekiem <- rbind(x10_uzDivniekiem, x10[2:3, ])
        x10_uzSeptini <- rbind(x10_uzSeptini, x10[4:10, ])
      } else {stop("processingTens: Desmitnieku tabulas pārdalei trūkst izstrādes koda. Rindas: ", r, " līdz ", r+9, "\n")}
    } else if(sum(x10$sak_beidz == "1") == 4) {
      if (all(x10$sak_beidz[c(1, 2)] == "2") && all(x10$zinkod[c(1, 2)] == "26") &&
          all(sapply(seq(3, 10, by = 2), function(i) all(diff(x10$NDZ_sanemsanas_datums[i:i+1]) == 0))) &&
          all(sapply(seq(1, 3, by = 2), function(i) all(diff(x10$NDZ_sanemsanas_datums[i:i+1]) != 0)))) {
        # Šis ir unikāla gadījuma, jo strādā ar tiem dubultajiem kodiem īslaicīgi kā fiziska persona, taču
        # 2023. gadā un 2022. gadā neviens sākšanas kods pirms tiem pirmiem trim 26, 26 un 25 kodiem nav atrodams.
        # Nu vispār pēc trīs gadiem neatlaiž par darba nesākšanu, bet ja atlaists ar 25 tad tur būs kļūda.
        # Labo, ja vajag savādāk, bet no pašreizējās informācijas, es šo griežu nost.
        x10_uzAstoniekiem <- rbind(x10_uzAstoniekiem, x10[3:10, ])
      } else if (all(x10$sak_beidz[c(3:4, 7, 9)] == "1") && 
                 all(sapply(seq(1, 10, by = 2), function(i) all(diff(x10$NDZ_sanemsanas_datums[i:i+1]) == 0))) &&
                 all(sapply(seq(2, 9, by = 2), function(i) all(diff(x10$NDZ_sanemsanas_datums[i:i+1]) != 0)))&&
                 x10$PS_code[1] == '__________' & x10$NM_code[1] == '__________') {
        p <- x10[1:6, ]
        p <- p[p$zinkod %in% c("40", "41"), ]
        x10_uzVieniniekiem <- rbind(x10_uzVieniniekiem, p[1, ])
        x10_uzDivniekiem <- rbind(x10_uzDivniekiem, p[2:3, ])
        rm(p)
      } else {stop("processingTens: Desmitnieku tabulas pārdalei trūkst izstrādes koda. Rindas: ", r, " līdz ", r+9, "\n")}
    } else if (sum(x10$sak_beidz == "1") == 6) {
      result <- processingTens_s6(x10)
    } else {
      stop("processingTens: Desmitnieku tabulas pārdalei trūkst izstrādes koda. Rindas: ", r, " līdz ", r+9, "\n")
    }
    
    if(exists("result")) {
      x10_uzVieniniekiem <- rbind(x10_uzVieniniekiem, result$x10_uzVieniniekiem)
      x10_uzDivniekiem <- rbind(x10_uzDivniekiem, result$x10_uzDivniekiem)
      x10_uzSeptini <- rbind(x10_uzSeptini, result$x10_uzSeptini)
      x10_uzAstoniekiem <- rbind(x10_uzAstoniekiem, result$x10_uzAstoniekiem)
      rm(result)
    }
    check_rows <- check_rows + 10
  }
  
#PĀRBAUDE: Vai rindu skaits no desmitniekiem atvasinātajās tabulās sakrīt ar rindām sākotnējā tabulā x.
if (nrow(x) == check_rows) {
    cat("PĀRBAUDE IZIETA: Apakštabulu rindu summa sakrīt ar izejošo desmitnieku tabulu.\n")
    rm(x, x10, r, check_rows)
} else {stop("PĀRBAUDE NAV IZIETA: Apakštabulu rindu summa NESAKRĪT ar izejošo desmitnieku tabulu.\n")}
  
#1 Apakštabulu x10_uzVieniniekiem sūta caur processingOnes().
if(nrow(x10_uzVieniniekiem) > 0) {
  cat("No desmitniekiem atvasināto tabulu x10_uzVieniniekiem pārsūta uz processingOnes() un tad uz tempNDZ, ko būvējam.\n")
  x10_uzVieniniekiem %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingOnes(o) %>% sendTo_tempNDZ()
} else {cat("Tabula x10_uzVieniniekiem ir tukša.\n")}
rm(x10_uzVieniniekiem) 
  
#2 Apakštabulu x10_uzDivniekiem sūta caur processingTwoes().
if(nrow(x10_uzDivniekiem) > 0) {
  cat("No desmitniekiem atvasinātā tabula x10_uzDivniekiem pārsūtīta uz processingTwoes un caur to uz tempNDZ, ko būvējam.\n")
  x10_uzDivniekiem %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingTwoes(o)
} else {cat("Tabula x10_uzDivniekiem ir tukša.\n")}
rm(x10_uzDivniekiem)
  
#3 Apakštabulu x10_uzSeptini sūta caur processingSeven().
if(nrow(x10_uzSeptini) > 0) {
  cat("No desmitniekiem atvasinātā tabula x10_uzSeptini pārsūtīta uz processingSeven un caur to uz tempNDZ, ko būvējam.\n")
  x10_uzSeptini %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingSeven(o)
} else {cat("Tabula x10_uzSeptini ir tukša.\n")}
rm(x10_uzSeptini)
  
#4 Apakštabulu x10_uzAstoniekiem sūta caur processingEights().
if(nrow(x10_uzAstoniekiem) > 0) {
  cat("No desmitniekiem atvasinātā tabula x10_uzAstoniekiem pārsūtīta uz processingEights un tad uz tempNDZ, ko būvējam.\n")
  x10_uzAstoniekiem %>% arrange(PS_code,, NM_code, NDZ_sanemsanas_datums) %>% processingEights(o)
} else {cat("Tabula x10_uzAstoniekiem ir tukša.\n")}
rm(x10_uzAstoniekiem) 
}

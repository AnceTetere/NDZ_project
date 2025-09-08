processingTwentyOne <- function(x, o, kods) {
  x <- x %>% arrange(PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
  x21_uz2 <- data.frame(); x21_uz15 <- data.frame()
  cR <- 0
  
  for (r in seq(1, nrow(x), by = 21)) {
    x21 <- x[r:(r + 20), ] %>% arrange(PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
    
    if (all(x21$sak_beidz[1:7] == c("1", "2", "2", "2", "2", "2", "1"))) {
      if (all(diff(x21$NDZ_sanemsanas_datums[1:7]) != 0)) {
        if (x21$period[1] == '______' && x21$PS_code[1] ==  '______________' && x21$NM_code[1] ==  '______________') {
          x21_uz2 <- x21[c(1,6),]; x21_uz15 <- x21[-(1:6),]
          if (kods %in% c("40", "50", "53") && o == "21") {ZERO_minus(x21 %>% slice(1))}
        } else {stop("26-nieku skriptā iztrūkst apstrādes koda. \n")}  
      } else {stop("26-nieku skriptā iztrūkst apstrādes koda. \n")}
    } else {stop("26-nieku skriptā iztrūkst apstrādes koda. \n")}
    
    cR <- cR + 21
  }
  
  #PĀRBAUDE: Vai rindu skaits no 21-niekiem atvasinātajās tabulās sakrīt ar rindām izejas tabulā x.
  if (cR == nrow(x)) {
    cat("PĀRBAUDE IZIETA: Apakštabulu rindu summa sakrīt ar sākuma 21-nieku tabulu. \n")
    rm(x, x21, r)
  } else {stop("PĀRBAUDE NAV IZIETA: Apakštabulu rindu summa NESAKRĪT ar sākotnējo 21-nieku tabulu. \n")}
  
  #1 Apakštabulu x21_uz2 sūta caur processingTwoes().
  if (nrow(x21_uz2) > 0) {
      x21_uz2 %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingTwoes(o, kods)
  } else {cat("Tabula x21_uz2 ir tukša.\n")}
  rm(x21_uz2)
  
  #2 Apakštabulu x21_uz15 sūta caur processingFifteen().
  if (nrow(x21_uz15) > 0) {
    x21_uz15 %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingFifteen(o, kods)
  } else {cat("Tabula x21_uz15 ir tukša.\n")}
  rm(x21_uz15)
}

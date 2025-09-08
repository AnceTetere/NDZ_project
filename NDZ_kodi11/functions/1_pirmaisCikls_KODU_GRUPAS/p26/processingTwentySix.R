processingTwentySix <- function(x, o, kods) {
  x <- x %>% arrange(PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
  x26_uz1 <- data.frame(); x26_uz21 <- data.frame()
  cR <- 0
  
  for (r in seq(1, nrow(x), by = 26)) {
    x26 <- x[r:(r + 25), ] %>% arrange(PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
    
    if (all(x26$sak_beidz[1:6] == c("2", "2", "2", "2", "2", "1"))) {
      if (all(diff(x26$NDZ_sanemsanas_datums[1:6]) != 0)) {
        if (x26$period[1] == '______' && x26$PS_code[1] ==  '______________' && x26$NM_code[1] ==  '______________') {
             x26_uz1 <- x26[5,]; x26_uz21 <- x26[-(1:5),]
             if (kods %in% c("40", "50", "53") && o == "26") {ZERO_minus(x26 %>% slice(1))}
        } else {stop("26-nieku skriptā iztrūkst apstrādes koda. \n")}  
      } else {stop("26-nieku skriptā iztrūkst apstrādes koda. \n")}
    } else {stop("26-nieku skriptā iztrūkst apstrādes koda. \n")}
    
    cR <- cR + 26
  }
  
  #PĀRBAUDE: Vai rindu skaits no 26-niekiem atvasinātajās tabulās sakrīt ar rindām izejas tabulā x.
  if (cR == nrow(x)) {
    cat("PĀRBAUDE IZIETA: Apakštabulu rindu summa sakrīt ar sākuma 26-nieku tabulu. \n")
    rm(x, x26, r)
  } else {stop("PĀRBAUDE NAV IZIETA: Apakštabulu rindu summa NESAKRĪT ar sākotnējo 26-nieku tabulu. \n")}
  
  #1 Apakštabulu x26_uz1 sūta caur processingOnes().
  if (nrow(x26_uz1) > 0) {
    x26_uz1 %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums, sak_beidz) %>% 
      processingOnes(o) %>% sendTo_tempNDZ(o)
  } else {cat("Tabula x26_uz1 ir tukša.\n")}
  rm(x26_uz1)
  
  #2 Apakštabulu x26_uz21 sūta caur processingTwentyOne().
  if (nrow(x26_uz21) > 0) {
    x26_uz21 %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingTwentyOne(o, kods)
  } else {cat("Tabula x26_uz21 ir tukša.\n")}
  rm(x26_uz21)
} 


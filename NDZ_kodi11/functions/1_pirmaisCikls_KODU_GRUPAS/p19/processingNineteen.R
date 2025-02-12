processingNineteen <- function(x, o, kods) {
  x <- x %>% arrange(PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
  x19_uz2 <- data.frame(); x19_uz17 <- data.frame()
  cR <- 0
  
  for (r in seq(1, nrow(x), by = 19)) {
    x19 <- x[r:(r + 18), ] %>% arrange(PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
    
    if (all(x19$sak_beidz[1:6] == c("1", "2", "1", "2", "1", "2"))) {
      if (all(diff(x19$NDZ_sanemsanas_datums[1:6]) != 0)) {
        if (x19$period[1] == "_________" && x19$PS_code[1] == "________" && x19$NM_code[1] == "________") {
          x19_uz2 <- rbind(x19_uz2, x19[1:2,])
          x19_uz17 <- rbind(x19_uz17, x19[3:19,])
        } else {stop("19-nieku skriptā iztrūkst apstrādes koda. \n")}  
      } else {stop("19-nieku skriptā iztrūkst apstrādes koda. \n")}
    } else {stop("19-nieku skriptā iztrūkst apstrādes koda. \n")}
    
    cR <- cR + 19
  }
  
  #PĀRBAUDE: Vai rindu skaits no 19-niekiem atvasinātajās tabulās sakrīt ar rindām izejas tabulā x.
  if (cR == nrow(x)) {
    cat("PĀRBAUDE IZIETA: Apakštabulu rindu summa sakrīt ar sākuma 19-nieku tabulu. \n")
    rm(x, x19, r)
  } else {stop("PĀRBAUDE NAV IZIETA: Apakštabulu rindu summa NESAKRĪT ar sākotnējo 19-nieku tabulu. \n")}
  
  #1 Apakštabulu x19_uz2 sūta caur processingTwoes().
  if (nrow(x19_uz2) > 0) {
    x19_uz2 %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingTwoes(o, kods)
  } else {cat("Tabula x19_uz2 ir tukša.\n")}
  rm(x19_uz2)
  
  #2 Apakštabulu x19_uz17 uz sūta caur processingSeventeen().
  if (nrow(x19_uz17) > 0) {
    x19_uz17 %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingSeventeen(o, kods)
  } else {cat("Tabula x19_uz17 ir tukša. \n")}
  rm(x19_uz17)
}

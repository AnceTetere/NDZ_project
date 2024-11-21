processingEighteen <- function(x, o) {
  x <- x %>% arrange(PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
  x18_uzDivi <- data.frame(); x18_uzSespadsmit <- data.frame()
  
  for (r in seq(1, nrow(x), by = 18)) {
    x18 <- x[r:(r + 17), ] %>% arrange(PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
    
    if (all(x18$sak_beidz == c("2", "1", "1", "2", "1", "2", "1", "2", "1", "2", "1", "2", "1", "2", "1", "2", "1", "2"))) {
      if (diff(x18$NDZ_sanemsanas_datums[1:2]) == 0 && all(diff(x18$NDZ_sanemsanas_datums[2:18]) != 0)) {
        if (x18$PS_code[1] == "_____________" && x18$NM_code[1] == "_____________") {
          x18_uzDivi <- rbind(x18_uzDivi, x18[c(2,1),])
          x18_uzSespadsmit <- rbind(x18_uzSespadsmit, x18[3:18, ])
        } else {stop("18-nieku skriptā iztrūkst apstrādes koda. \n")}
      } else {stop("18-nieku skriptā iztrūkst apstrādes koda. \n")}
    } else {stop("18-nieku skriptā iztrūkst apstrādes koda. \n")}
  }
  
  #PĀRBAUDE: Vai rindu skaits no 18-niekiem atvasinātajās tabulās sakrīt ar rindām izejas tabulā x.
  if ((nrow(x18_uzDivi) + nrow(x18_uzSespadsmit)) == nrow(x)) {
    cat("PĀRBAUDE IZIETA: Apakštabulu rindu summa sakrīt ar sākuma 18-nieku tabulu. \n")
    rm(x, x18, r)
  } else {stop("PĀRBAUDE NAV IZIETA: Apakštabulu rindu summa NESAKRĪT ar sākotnējo 18-nieku tabulu. \n")}
  
  #1 Apakštabulu x18_uzDivi sūta caur processingTwoes().
  if (nrow(x18_uzDivi) > 0) {
    cat("No 18-niekiem atvasinātā tabula x18_uzDivi pārsūtīta uz processingTwoes() un caur to uz tempNDZ, ko būvējam.\n")
    x18_uzDivi %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingTwoes(o)
  } else {cat("Tabula x18_uzDivi ir tukša.\n")}
  rm(x18_uzDivi)
  
  #2 Apakštabulu x18_uzSespadsmit uz sūta caur processingSixteen().
  if (nrow(x18_uzSespadsmit) > 0) {
    cat("No 18-niekiem atvasinātā tabula x18_uzSespadsmit pārsūtīta uz processingSixteen() un tad caur to uz tempNDZ, ko būvējam. \n")
    x18_uzSespadsmit %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingSixteen(o)
  } else {cat("Tabula x18_uzSespadsmit ir tukša. \n")}
  rm(x18_uzSespadsmit)
}

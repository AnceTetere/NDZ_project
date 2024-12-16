processingSixteen <- function(x, o) {
  x <- x %>% arrange(PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
  x16_uz1 <- data.frame()
  x16_uzDivniekiem <- data.frame()
  
  for (r in seq(1, nrow(x), by = 16)) {
    x16 <- x[r:(r+15), ] %>% arrange(PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
    
    if (all(x16$sak_beidz == c("1", "2", "1", "2", "1", "2", "1", "2", "1", "2", "1", "2", "1", "2", "1", "2"))) {
      if (all(diff(x16$NDZ_sanemsanas_datums) != 0)) {
        x16_uzDivniekiem <- rbind(x16_uzDivniekiem, x16)
      } else {stop("16-nieku tabulā trūkst izstrādes koda. Rindas: ", r, ":", r+15, "\n")}
    } else if (all(x16$sak_beidz == c("2", "1", "2", "1", "2", "1", "2", "1", "2", "1", "2", "1", "2", "1", "2", "1"))) {
      if (all(diff(x16$NDZ_sanemsanas_datums) != 0)) {
        x16_uz1 <- rbind(x16_uz1,x16[c(1,16), ])
        x16_uzDivniekiem <- rbind(x16_uzDivniekiem, x16[2:15, ])
      } else {stop("16-nieku tabulā trūkst izstrādes koda. Rindas: ", r, ":", r+15, "\n")}
    } else {stop("16-nieku tabulā trūkst izstrādes koda. Rindas: ", r, ":", r+15, "\n")}
  }
  
  #PĀRBAUDE: Vai rindu skaits no 16-niekiem atvasinātajās tabulās sakrīt ar rindām izejas tabulā x.
  if (nrow(x16_uz1) + nrow(x16_uzDivniekiem) == nrow(x)) {
    cat("PĀRBAUDE IZIETA: Apakštabulu rindu summa sakrīt ar izejošo 16-nieku tabulu.\n")
    rm(x, x16, r)
  } else {stop("PĀRBAUDE NAV IZIETA: Apakštabulu rindu summa NESAKRĪT ar izejošo 16-nieku tabulu.\n")}
  
  #1 Apakštabulu x16_uz1 sūta caur processingOnes().
  if (nrow(x16_uz1) > 0) {
    x16_uz1 <- x16_uz1 %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% 
      processingOnes(o) %>%  sendTo_tempNDZ()
  } else {cat("Tabula x16_uz1 ir tukša.\n")}
  rm(x16_uz1)
  
  #2 Apakštabulu x16_uzDivniekiem sūta caur processingTwoes().
  if (nrow(x16_uzDivniekiem) > 0) {
    x16_uzDivniekiem %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingTwoes(o) 
  } else {cat("Tabula x16_uzDivniekiem ir tukša.\n")}
  rm(x16_uzDivniekiem)
  }

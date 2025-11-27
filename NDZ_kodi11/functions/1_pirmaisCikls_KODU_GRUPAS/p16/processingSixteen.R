processingSixteen <- function(x, o, kods) {
  x <- x %>% arrange(PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
  x16_uz1 <- data.frame(); x16_uz2 <- data.frame(); x16_uz6 <- data.frame(); x16_uz14 <- data.frame(); x16_uz15 <- data.frame()
  cR <- 0

  for (r in seq(1, nrow(x), by = 16)) {
    x16 <- x[r:(r+15), ] %>% arrange(PS_code, DN_code, NM_code, NDZ_sanemsanas_datums, zinkod)
    
    if (sum(x16$sak_beidz == "1") == 8) {
      if (x16$sak_beidz[1] == "1") {
        if (x16$sak_beidz[2] == "2") {
          if (x16$sak_beidz[3] == "1") {
            if (x16$sak_beidz[4] == "2") {    
              if (all(diff(x16$NDZ_sanemsanas_datums[1:3]) != 0)) {
                         #IZIETAS 10-nieku PĀRBAUDES
                          x16_uz2 <- rbind(x16_uz2, x16[1:2, ]); x16_uz14 <- rbind(x16_uz14, x16[3:16, ])
              } else if (all(sapply(c(1,3), function(i) diff(x16$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) && diff(x16$NDZ_sanemsanas_datums[2:3]) != 0) {
                         #JO PIRMOREIZ
                         if ((x16$period[1] == '______' && x16$PS_code[1] ==  '______________' && x16$NM_code[1] ==  '______________') ||
                             (x16$period[1] == '______' && x16$PS_code[1] ==  '______________' && x16$NM_code[1] ==  '______________') ||
                             (x16$period[1] == '______' && x16$PS_code[1] ==  '______________' && x16$NM_code[1] ==  '______________') ||
                             (x16$period[1] == '______' && x16$PS_code[1] ==  '______________' && x16$NM_code[1] ==  '______________') ||
                             (x16$period[1] == '______' && x16$PS_code[1] ==  '______________' && x16$NM_code[1] ==  '______________') ||
                             (x16$period[1] == '______' && x16$PS_code[1] ==  '______________' && x16$NM_code[1] ==  '______________') ||
                             (x16$period[1] == '______' && x16$PS_code[1] ==  '______________' && x16$NM_code[1] ==  '______________')) {
                                  x16_uz2 <- rbind(x16_uz2, x16[1:2, ]); x16_uz14 <- rbind(x16_uz14, x16[3:16, ])
                         } else {stop("16-nieku tabulā trūkst izstrādes koda. \n")}
              } else if (diff(x16$NDZ_sanemsanas_datums[1:2]) == 0 && all(diff(x16$NDZ_sanemsanas_datums[2:4]) != 0)) {
                         #JO PIRMOREIZ
                         if ((x16$period[1] == '______' && x16$PS_code[1] ==  '______________' && x16$NM_code[1] ==  '______________') ||
                             (x16$period[1] == '______' && x16$PS_code[1] ==  '______________' && x16$NM_code[1] ==  '______________') ||
                             (x16$period[1] == '______' && x16$PS_code[1] ==  '______________' && x16$NM_code[1] ==  '______________') ||
                             (x16$period[1] == '______' && x16$PS_code[1] ==  '______________' && x16$NM_code[1] ==  '______________') ||
                             (x16$period[1] == '______' && x16$PS_code[1] ==  '______________' && x16$NM_code[1] ==  '______________') ||
                             (x16$period[1] == '______' && x16$PS_code[1] ==  '______________' && x16$NM_code[1] ==  '______________')) {
                              x16_uz2 <- rbind(x16_uz2, x16[1:2, ]); x16_uz14 <- rbind(x16_uz14, x16[3:16, ])
                        } else {stop("16-nieku tabulā trūkst izstrādes koda. \n")}
              } else {stop("16-nieku tabulā trūkst izstrādes koda. \n")}
            } else {stop("16-nieku tabulā trūkst izstrādes koda. \n")}
          } else {stop("16-nieku tabulā trūkst izstrādes koda. \n")}
        } else {stop("16-nieku tabulā trūkst izstrādes koda. \n")}
      } else if (x16$sak_beidz[1] == "2") {
               if (x16$sak_beidz[2] == "1") {
                 if (x16$sak_beidz[3] == "2") {
                   if (x16$sak_beidz[4] == "1") {    
                     if (all(diff(x16$NDZ_sanemsanas_datums[1:3]) != 0)) {
                        #JO PIRMOREIZ
                        if ((x16$period[1] == '______' && x16$PS_code[1] ==  '______________' && x16$NM_code[1] ==  '______________') ||
                            (x16$period[1] == '______' && x16$PS_code[1] ==  '______________' && x16$NM_code[1] ==  '______________') ||
                            (x16$period[1] == '______' && x16$PS_code[1] ==  '______________' && x16$NM_code[1] ==  '______________') ||
                            (x16$period[1] == '______' && x16$PS_code[1] ==  '______________' && x16$NM_code[1] ==  '______________') ||
                            (x16$period[1] == '______' && x16$PS_code[1] ==  '______________' && x16$NM_code[1] ==  '______________') ||
                            (x16$period[1] == '______' && x16$PS_code[1] ==  '______________' && x16$NM_code[1] ==  '______________') ||
                            (x16$period[1] == '______' && x16$PS_code[1] ==  '______________' && x16$NM_code[1] ==  '______________') ||
                            (x16$period[1] == '______' && x16$PS_code[1] ==  '______________' && x16$NM_code[1] ==  '______________') ||
                            (x16$period[1] == '______' && x16$PS_code[1] ==  '______________' && x16$NM_code[1] ==  '______________')) {
                             x16_uz1 <- rbind(x16_uz1, x16[1, ]); x16_uz15 <- rbind(x16_uz15, x16[-1, ])
                        } else {stop("16-nieku tabulā trūkst izstrādes koda. \n")}
              } else {stop("16-nieku tabulā trūkst izstrādes koda. \n")}
            } else {stop("16-nieku tabulā trūkst izstrādes koda. \n")}
          } else if (x16$sak_beidz[3] == "1") {
                   if (x16$sak_beidz[4] == "2") {    
                     if (diff(x16$NDZ_sanemsanas_datums[1:2]) == 0 && all(diff(x16$NDZ_sanemsanas_datums[2:5]) != 0)) {
                       #JO PIRMOREIZ
                       if (x16$period[1] == '______' && x16$PS_code[1] ==  '______________' && x16$NM_code[1] ==  '______________') {
                         x16_uz2 <- rbind(x16_uz2, x16[c(2,1), ]); x16_uz14 <- rbind(x16_uz14, x16[3:16, ])
                       } else {stop("16-nieku tabulā trūkst izstrādes koda. \n")}
                     } else {stop("16-nieku tabulā trūkst izstrādes koda. \n")}
                   } else {stop("16-nieku tabulā trūkst izstrādes koda. \n")}
                 } else {stop("16-nieku tabulā trūkst izstrādes koda. \n")}
               } else if (x16$sak_beidz[2] == "2") {
                        if (x16$sak_beidz[3] == "1") {
                          if (kods == "40") {
                            if (all(x16$sak_beidz[4:16] == c("1", "2", "2", "1", "1", "2", "2", "1", "1", "2", "2", "1", "1"))) {
                              if (all(sapply(seq(1,10,by=2), function(i) diff(x16$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) && 
                                  all(sapply(seq(2,10,by=2), function(i) diff(x16$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
                                #JO PIRMOREIZ
                                   if (x16$period[1] == '______' && x16$PS_code[1] ==  '______________' && x16$NM_code[1] ==  '______________') {
                                       x16_uz1 <- rbind(x16_uz1, x16[1, ]); 
                                       x16_uz2 <- rbind(x16_uz2, x16[c(3,5,7,9), ]); x16_uz6 <- rbind(x16_uz6, x16[11:16, ])
                                   } else {stop("16-nieku tabulā trūkst izstrādes koda. \n")}
                                } else {stop("16-nieku tabulā trūkst izstrādes koda. \n")}
                     } else {stop("16-nieku tabulā trūkst izstrādes koda. \n")}
                   } else {stop("16-nieku tabulā trūkst izstrādes koda. \n")}
                 } else {stop("16-nieku tabulā trūkst izstrādes koda. \n")}
               } else {stop("16-nieku tabulā trūkst izstrādes koda. \n")}
      } else {stop("16-nieku tabulā trūkst izstrādes koda. \n")}
    } else {stop("16-nieku tabulā trūkst izstrādes koda. \n")}

    if (x16$sak_beidz[1] == "1" && kods %in% c("40", "50", "53") && o == "16") {ZERO_minus(x16 %>% slice(1))}
    cR <- cR + 16
  }
  
  #PĀRBAUDE: Vai rindu skaits no 16-niekiem atvasinātajās tabulās sakrīt ar rindām izejas tabulā x.
  if (cR == nrow(x)) {
    cat("PĀRBAUDE IZIETA: Apakštabulu rindu summa sakrīt ar izejošo 16-nieku tabulu.\n")
    rm(x, x16, r)
  } else {stop("PĀRBAUDE NAV IZIETA: Apakštabulu rindu summa NESAKRĪT ar izejošo 16-nieku tabulu.\n")}
  
  #1 Apakštabulu x16_uz1 sūta caur processingOnes().
  if (nrow(x16_uz1) > 0) {
    x16_uz1 <- x16_uz1 %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% 
      processingOnes(o) %>% sendTo_tempNDZ(o)
  } else {cat("Tabula x16_uz1 ir tukša.\n")}
  rm(x16_uz1)
  
  #2 Apakštabulu x16_uz2 sūta caur processingTwoes().
  if (nrow(x16_uz2) > 0) {
    x16_uz2 %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingTwoes(o, kods) 
  } else {cat("Tabula x16_uz2 ir tukša.\n")}
  rm(x16_uz2)

  #3 Apakštabulu x16_uz6 sūta caur processingSixes().
  if (nrow(x16_uz6) > 0) {
    x16_uz6 %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingSixes(o, kods) 
  } else {cat("Tabula x16_uz6 ir tukša.\n")}
  rm(x16_uz6)
  
  #4 Apakštabulu x16_uz14 sūta caur processingFourteen().
  if (nrow(x16_uz14) > 0) {
    x16_uz14 %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingFourteen(o, kods) 
  } else {cat("Tabula x16_uz14 ir tukša.\n")}
  rm(x16_uz14)
  
  #5 Apakštabulu x16_uz15 sūta caur processingFifteen().
  if (nrow(x16_uz15) > 0) {
    x16_uz15 %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingFifteen(o, kods) 
  } else {cat("Tabula x16_uz15 ir tukša.\n")}
  rm(x16_uz15)
}


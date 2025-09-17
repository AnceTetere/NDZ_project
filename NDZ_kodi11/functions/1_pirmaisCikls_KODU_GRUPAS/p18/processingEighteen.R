processingEighteen <- function(x, o, kods) {
  x <- x %>% arrange(PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
  x18_uz1 <- data.frame(); x18_uzDivi <- data.frame(); x18_uz15 <- data.frame(); x18_uzSespadsmit <- data.frame()
  cR <- 0
  
  for (r in seq(1, nrow(x), by = 18)) {
    x18 <- x[r:(r + 17), ] %>% arrange(PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
    
    if (all(x18$sak_beidz[1:5] == c("2", "1", "1", "2", "1"))) {
            if (all(sapply(c(1,3), function(i) diff(x18$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) && 
                all(sapply(c(2,4,5), function(i) diff(x18$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
                if (x18$period[1] == '______' && x18$PS_code[1] ==  '______________' && x18$NM_code[1] ==  '______________') {
                    x18_uzDivi <- rbind(x18_uzDivi, x18[c(2,1),])
                    x18_uzSespadsmit <- rbind(x18_uzSespadsmit, x18[3:18, ])
                    if (kods %in% c("40", "50", "53") && kods == "18") {ZERO_plus(x18 %>% slice(18))}
              } else {stop("18-nieku skriptā iztrūkst apstrādes koda. \n")}
            } else if (diff(x18$NDZ_sanemsanas_datums[1:2]) == 0 && all(diff(x18$NDZ_sanemsanas_datums[2:5]) != 0)) {
                     #JO PIRMOREIZ
                     if (x18$period[1] == '______' && x18$PS_code[1] ==  '______________' && x18$NM_code[1] ==  '______________') {
                         x18_uzDivi <- rbind(x18_uzDivi, x18[c(2,1),])
                         x18_uzSespadsmit <- rbind(x18_uzSespadsmit, x18[3:18, ])
                         if (kods %in% c("40", "50", "53") && kods == "18") {ZERO_plus(x18 %>% slice(18))}
                    } else {stop("18-nieku skriptā iztrūkst apstrādes koda. \n")}
            } else {stop("18-nieku skriptā iztrūkst apstrādes koda. \n")}
    } else if (all(x18$sak_beidz[1:5] == c("2", "1", "2", "1", "2"))) {
            if (all(diff(x18$NDZ_sanemsanas_datums[1:5]) != 0)) {
              #Jo pirmoreiz
              if ((x18$period[1] == '______' && x18$PS_code[1] ==  '______________' && x18$NM_code[1] ==  '______________') ||
                  (x18$period[1] == '______' && x18$PS_code[1] ==  '______________' && x18$NM_code[1] ==  '______________') ||
                  (x18$period[1] == '______' && x18$PS_code[1] ==  '______________' && x18$NM_code[1] ==  '______________') ||
                  (x18$period[1] == '______' && x18$PS_code[1] ==  '______________' && x18$NM_code[1] ==  '______________')) {
                    x18_uz1 <- rbind(x18_uz1, x18[1,])
                    x18_uzDivi <- rbind(x18_uzDivi, x18[2:3,])
                    x18_uz15 <- rbind(x18_uz15, x18[4:18, ])
              } else {stop("18-nieku skriptā iztrūkst apstrādes koda. \n")}
            } else {stop("18-nieku skriptā iztrūkst apstrādes koda. \n")}
    } else if (all(x18$sak_beidz[1:5] == c("1", "2", "1", "2", "1"))) {
            if (all(diff(x18$NDZ_sanemsanas_datums[1:5]) != 0)) {
              #Jo pirmoreiz
              if ((x18$period[1] == '______' && x18$PS_code[1] ==  '______________' && x18$NM_code[1] ==  '______________') ||
                  (x18$period[1] == '______' && x18$PS_code[1] ==  '______________' && x18$NM_code[1] ==  '______________') ||
                  (x18$period[1] == '______' && x18$PS_code[1] ==  '______________' && x18$NM_code[1] ==  '______________')) {
                    x18_uzDivi <- rbind(x18_uzDivi, x18[1:2,])
                    x18_uzSespadsmit <- rbind(x18_uzSespadsmit, x18[3:18, ])
                    if (kods %in% c("40", "50", "53") && kods == "18") {ZERO_minus(x18 %>% slice(1))}
              } else {stop("18-nieku skriptā iztrūkst apstrādes koda. \n")}
            } else if (all(sapply(c(1,3,5), function(i) diff(x18$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
                       all(sapply(c(2,4), function(i) diff(x18$NDZ_sanemsanas_datums[i:(i+1)]) != 0)))   {
                       #Jo pirmoreiz
                        if (x18$period[1] == '______' && x18$PS_code[1] ==  '______________' && x18$NM_code[1] ==  '______________') {
                         x18_uzDivi <- rbind(x18_uzDivi, x18[1:2,])
                         x18_uzSespadsmit <- rbind(x18_uzSespadsmit, x18[3:18, ])
                        if (kods %in% c("40", "50", "53") && kods == "18") {ZERO_minus(x18 %>% slice(1))}
                        } else {stop("18-nieku skriptā iztrūkst apstrādes koda. \n")}
           } else {stop("18-nieku skriptā iztrūkst apstrādes koda. \n")}
    } else if (all(x18$sak_beidz[1:5] == c("1", "2", "1", "2", "2"))) {
            if (all(diff(x18$NDZ_sanemsanas_datums[1:5]) != 0)) {
              #Jo pirmoreiz
              if (x18$period[1] == '______' && x18$PS_code[1] ==  '______________' && x18$NM_code[1] ==  '______________') {
                x18_uzDivi <- rbind(x18_uzDivi, x18[1:2,])
                x18_uzSespadsmit <- rbind(x18_uzSespadsmit, x18[3:18, ])
                if (kods %in% c("40", "50", "53") && kods == "18") {ZERO_minus(x18 %>% slice(1))}
              } else {stop("18-nieku skriptā iztrūkst apstrādes koda. \n")}
            } else if (all(diff(x18$NDZ_sanemsanas_datums[1:5]) != 0)) {
              #Jo pirmoreiz
              if (x18$period[1] == '______' && x18$PS_code[1] ==  '______________' && x18$NM_code[1] ==  '______________') {
                x18_uzDivi <- rbind(x18_uzDivi, x18[1:2,])
                x18_uzSespadsmit <- rbind(x18_uzSespadsmit, x18[3:18, ])
                if (kods %in% c("40", "50", "53") && kods == "18") {ZERO_minus(x18 %>% slice(1))}
              } else {stop("18-nieku skriptā iztrūkst apstrādes koda. \n")}
            } else {stop("18-nieku skriptā iztrūkst apstrādes koda. \n")}
    } else {stop("18-nieku skriptā iztrūkst apstrādes koda. \n")}
    cR <- cR + 18
}

  #PĀRBAUDE: Vai rindu skaits no 18-niekiem atvasinātajās tabulās sakrīt ar rindām izejas tabulā x.
  if (cR == nrow(x)) {
    cat("PĀRBAUDE IZIETA: Apakštabulu rindu summa sakrīt ar sākuma 18-nieku tabulu. \n")
    rm(x, x18, r)
  } else {stop("PĀRBAUDE NAV IZIETA: Apakštabulu rindu summa NESAKRĪT ar sākotnējo 18-nieku tabulu. \n")}
  
  #1 Apakštabulu x18_uz1 sūta caur processingOnes().
    if (nrow(x18_uz1) > 0) {
      x18_uz1 %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingOnes(o) %>% sendTo_tempNDZ(o)
    } else {cat("Tabula x18_uz1 ir tukša.\n")}
    rm(x18_uz1)
    
  #2 Apakštabulu x18_uzDivi sūta caur processingTwoes().
  if (nrow(x18_uzDivi) > 0) {
    x18_uzDivi %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingTwoes(o, kods)
  } else {cat("Tabula x18_uzDivi ir tukša.\n")}
  rm(x18_uzDivi)
  
  #3 Apakštabulu x18_uz15 uz sūta caur processingFifteen().
  if (nrow(x18_uz15) > 0) {
    x18_uz15 %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingFifteen(o, kods)
  } else {cat("Tabula x18_uz15 ir tukša. \n")}
  rm(x18_uz15)
  
  #4 Apakštabulu x18_uzSespadsmit uz sūta caur processingSixteen().
  if (nrow(x18_uzSespadsmit) > 0) {
    x18_uzSespadsmit %>% arrange(PS_code, NM_code, NDZ_sanemsanas_datums) %>% processingSixteen(o, kods)
  } else {cat("Tabula x18_uzSespadsmit ir tukša. \n")}
  rm(x18_uzSespadsmit)
}

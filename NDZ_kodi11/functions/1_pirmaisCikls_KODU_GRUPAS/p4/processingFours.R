processingFours <- function(x, o, kods) {
  x <- x %>% arrange(PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)

  x4_uzVieniniekiem <- data.frame(); x4_trueDoubles <- data.frame(); x4_uzTrijniekiem <- data.frame()
  check_rows <- 0
  
  result <- function(r) {
    x4_uzVieniniekiem <<- rbind(x4_uzVieniniekiem, r$x4_uzVieniniekiem)
    x4_trueDoubles <<- rbind(x4_trueDoubles, r$x4_trueDoubles)
    x4_uzTrijniekiem <<- rbind(x4_uzTrijniekiem, r$x4_uzTrijniekiem)
    rm(r)
  }

for (r in seq(1, nrow(x), by = 4)) {
  x4 <- x[r:(r + 3), ] %>% arrange(PS_code, DN_code, NM_code, NDZ_sanemsanas_datums, sak_beidz)
  
  if(!(doublesTest(1, x4) && doublesTest(3, x4))) {
    stop("Četrinieku apstrādes tabulā, ko izstrādā caur funkciju processingFours(), 
               rindās no", r, "līdz", r + 3, "nesakrīt pseidokoda, NM_code, DN_code, period kombinācija visās četrās rindās.")
  } else if (all(x4$sak_beidz == c("2", "1", "2", "2"))) {
            result(processingFours_2122(x4))
            if (kods %in% c("40", "50", "53") && o == "4") {ZERO_plus(x4 %>% slice(4))}
  } else if (all(x4$sak_beidz == c("1", "2", "1", "2"))) {
            x4_trueDoubles <- rbind(x4_trueDoubles, x4)
            if (kods %in% c("40", "50", "53") && o == "4") {ZERO_plus(x4 %>% slice(4)); ZERO_minus(x4 %>% slice(1))}
  } else if (all(x4$sak_beidz == c("1", "1", "2", "2"))) {
            result(processingFours_1122(x4, o, kods))
  } else if (all(x4$sak_beidz == c("2", "1", "2", "1"))) {
            result(processingFours_2121(x4))
  } else if (all(x4$sak_beidz == c("1", "1", "2", "1"))) {
            result(processingFours_1121(x4, o, kods))
  } else if (all(x4$sak_beidz == c("1", "2", "1", "1"))) {
            result(processingFours_1211(x4, o, kods))  
            if (kods %in% c("40", "50", "53") && o == "4") {ZERO_minus(x4 %>% slice(1))}
 # } else if (all(x4$sak_beidz == "1") || all(x4$sak_beidz == "2")) {
#            x4_uzVieniniekiem <- rbind(x4_uzVieniniekiem, codes_match(x4[1, ]))
#            if (kods %in% c("40", "50", "53") && o == "4") {ZERO_minus(x4 %>% slice(1))}
  } else if (all(x4$sak_beidz == c("1", "1", "1", "2"))) {
            x4_trueDoubles <- rbind(x4_trueDoubles, x4[3:4, ])
            if (kods %in% c("40", "50", "53") && o == "4") {ZERO_plus(x4 %>% slice(4)); ZERO_minus(x4 %>% slice(1))}
  } else if (all(x4$sak_beidz == c("1", "2", "2", "1"))) {
            result(processingFours_1221(x4))
            if (kods %in% c("40", "50", "53") && o == "4") {ZERO_minus(x4 %>% slice(1))}
  } else if (all(x4$sak_beidz == c("2", "1", "1", "1"))) {
            result(processingFours_2111(x4))
  } else if (all(x4$sak_beidz == c("2", "2", "1", "1"))) {
            result(processingFours_2211(x4))
  } else if (all(x4$sak_beidz == c("2", "1", "2", "2"))) {
            result(processingFours_2122(x4))
            if (kods %in% c("40", "50", "53") && o == "4") {ZERO_plus(x4 %>% slice(4))}
  } else if (all(x4$sak_beidz == c("1", "2", "2", "2"))) {
            if ("26" %in% x4$zinkod) {
              x4_trueDoubles <- rbind(x4_trueDoubles, x4[x4$sak_beidz == "1" | x4$zinkod == "26", ])
            } else if(all(diff(x4$NDZ_sanemsanas_datums) != 0)){
              x4_trueDoubles <- rbind(x4_trueDoubles, x4[c(1,4), ])
              if (kods %in% c("40", "50", "53") && o == "4") {ZERO_plus(x4 %>% slice(4)); ZERO_minus(x4 %>% slice(1))}
            } else {stop("Šeit četrinieku izstrādes tabulas rindām ", r, " līdz ", r+3, " trūkst apstrādes koda.")}
    } else if (all(x4$sak_beidz == c("2", "1", "1", "2"))) { 
              result(processingFours_2112(x4))
              if (kods %in% c("40", "50", "53") && o == "4") {ZERO_plus(x4 %>% slice(4))}
    } else if (all(x4$sak_beidz == c("2", "2", "1", "2"))) { 
              result(processingFours_2212(x4, o, kods))
    } else if (all(x4$sak_beidz == c("2", "2", "2", "1"))) { 
              result(processingFours_2221(x4, o, kods))
    } else {stop(cat("Šeit četrinieku izstrādes tabulas rindām", r, "līdz", r+3, "trūkst apstrādes koda."))}
  check_rows <- check_rows + 4
}


#PĀRBAUDE
if(nrow(x) == check_rows) {
    cat("PĀRBAUDE IZIETA: Četrinieku tabula veiksmīgi pārdalījusies apakštabulās.\n"); rm(x, check_rows, r, x4)
} else {stop("processingFours 
                    Četrinieku tabula NAV pārdalījusies.
                    check_rows cipars nesakrīt ar rindu skaitu izejas tabulā.\n")}

#1 Atvasināto tabulu x4_uzVieniniekiem sūta uz vieninieku apstrādi
if(nrow(x4_uzVieniniekiem) > 0) {
  cat(sendTo_tempNDZ(processingOnes(x4_uzVieniniekiem, o), o))
} else {cat("No četriniekiem pārsūtāmajā vieninieku tabulā nebija nevienas rindas.\n")}
rm(x4_uzVieniniekiem)
    
#2 Atvasināto tabulu x4_trueDoubles sūta caur processingTwoes().
if(nrow(x4_trueDoubles) > 0) {
  cat(processingTwoes(x4_trueDoubles, o, kods))
} else {cat("No četriniekiem pārsūtāmajā divnieku tabulā nebija nevienas rindas.\n")}
rm(x4_trueDoubles)

#3 Atvasināto tabulu x4_uzTrijniekiem sūta caur processingThrees().
if(nrow(x4_uzTrijniekiem) > 0) {
  processingThrees(x4_uzTrijniekiem, o, kods)
} else {cat("No četriniekiem pārsūtāmajā trijnieku tabulā nebija nevienas rindas.\n")}
rm(x4_uzTrijniekiem)
}


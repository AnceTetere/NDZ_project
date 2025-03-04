processingNines_1212 <- function(a, o, kods) {  
  a <- a %>% arrange(PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
  a1 <- data.frame(); a2 <- data.frame(); a6 <- data.frame(); a7 <- data.frame(); a8 <- data.frame()

  if (all(a$sak_beidz == c("1", "2", "1", "2", "1", "2", "1", "2", "1")) && 
      diff(a$NDZ_sanemsanas_datums[1:2]) == 0 && all(diff(a$NDZ_sanemsanas_datums[2:5]) != 0)) {
          a1 <- rbind(a1, a[9, ])
          a8 <- rbind(a8, a[1:8, ])
          if (kods %in% c("40", "50", "53") && o == "9") {ZERO_minus(a %>% slice(1))}
  } else if (all(a$sak_beidz == c("1", "2", "1", "2", "2", "1", "1", "2", "1")) && 
             all(sapply(seq(1, 7, by = 2), function(i) all(diff(a$NDZ_sanemsanas_datums[i:i+1]) == 0))) &&
             all(sapply(seq(2, 8, by = 2), function(i) all(diff(a$NDZ_sanemsanas_datums[i:(i+1)]) != 0)))) {
    a1 <- rbind(a1, a[9, ])
    a8 <- rbind(a8, a[1:8, ])
    if (kods %in% c("40", "50", "53") && o == "9") {ZERO_minus(a %>% slice(1))}
  } else if (all(a$sak_beidz == c("1", "2", "1", "2", "1", "2", "1", "2", "1"))) {
    a1 <- rbind(a1, a[9, ])
    a8 <- rbind(a8, a[1:8, ])
    if (kods %in% c("40", "50", "53") && o == "9") {ZERO_minus(a %>% slice(1))}
  } else if (all(diff(a$NDZ_sanemsanas_datums[1:4]) != 0)) {
         a2 <- rbind(a2, a[1:2, ])
         a7 <- rbind(a7, a[3:9, ])
           if (kods %in% c("40", "50", "53") && o == "9") {ZERO_minus(a %>% slice(1))}
  } else {stop("processingNines_1212 iztrūkst apstrādes kods. \n")}
  
  rm(a, kods)
  return(list(a_uzVieniniekiem = a1,
              a_uzDivi = a2,
              a_uzSesi = a6,
              a_uzSeptini = a7,
              a_uzAstoniekiem = a8))
}

processingNines_1221 <- function(a, o, kods) {  
  #a <- a for testing
  a <- a %>% arrange(PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
  a1 <- data.frame(); a2 <- data.frame(); a6 <- data.frame(); a7 <- data.frame(); a8 <- data.frame()
  
  if (diff(a$NDZ_sanemsanas_datums[1:2]) != 0 && diff(a$NDZ_sanemsanas_datums[8:9]) != 0) {
    a1 <- rbind(a1, a[9, ])
    a8 <- rbind(a8, a[1:8, ])
    if (kods %in% c("40", "50", "53") && o == "9") {ZERO_minus(a %>% slice(1))}
  } else if (all(sapply(c(2,4,8), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) && diff(a$NDZ_sanemsanas_datums[1:2]) != 0) {
    #JO PIRMOREIZ
    if (a$period[1] == "_________" && a$PS_code[1] == "_________" && a$NM_code[1] == "_________") {
      a1 <- rbind(a1, a[9, ])
      a8 <- rbind(a8, a[1:8, ])
      if (kods %in% c("40", "50", "53") && o == "9") {ZERO_minus(a %>% slice(1))}
    } else {stop("processingNines_1221 iztrūkst apstrādes kods. \n")}
  } else {stop("processingNines_1221 iztrūkst apstrādes kods. \n")}
  } else {stop("processingNines_1221: Trūkst apstrādes koda.")}
  
  rm(a, kods)
  return(list(x9_uzVieniniekiem = a1,
              x9_uzDivi = a2,
              x9_uzSesi = a6,
              x9_uzSeptini = a7,
              x9_uzAstoniekiem = a8))
  
}

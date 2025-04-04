processingFives_s2e3 <- function(a, o, kods) {  
  #a <- x5s2 for testing
  a <- a %>% arrange(PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
  a1 <- data.frame(); a2 <- data.frame(); a4 <- data.frame()
  
  if (all(a$sak_beidz[c(2,4)] == "1") && a$zinkod[3] == "26") {
    a1 <- rbind(a1, a[1, ])
    a4 <- rbind(a4, a[-1, ])
  } else if (all(a$sak_beidz[c(1,4)] == "1")) {
    a2 <- rbind(a2, a[-3, ])
    if (kods %in% c("40", "50", "53") && o == "5") {ZERO_plus(a %>% slice(5)); ZERO_minus(a %>% slice(1))}
  } else if (all(a$sak_beidz[c(2,4)] == "1")) {
          if ((a$period[1] == "_____" && a$PS_code[1] == '_________' && a$NM_code[1] == '_____') ||
              (a$period[1] == "_____" && a$PS_code[1] == '_____' && a$NM_code[1] == '_____') ||
              (a$period[1] == "_____" && a$PS_code[1] == '_________' && a$NM_code[1] == '_____')) {
            a1 <- rbind(a1, a[1, ])
            a2 <- rbind(a2, a[-1, ])
            if (kods %in% c("40", "50", "53") && o == "5") {ZERO_plus(a %>% slice(5))}
          } else {stop("processingFives_s2e3: Trūkst apstrādes koda.")}
  } else if (all(a$sak_beidz[2:3] == "1")) {
          if (kods == "40")  {
            if (a$period[1] == "_____" && a$PS_code[1] == '_________' && a$NM_code[1] == '_____') {
              a1 <- rbind(a1, a[1, ])
              a2 <- rbind(a2, a[c(3,5), ])
            } else {stop("processingFives_s2e3: Trūkst apstrādes koda.")}
            if (kods %in% c("40", "50", "53") && o == "5") {ZERO_plus(a %>% slice(5))}
          } else if ((a$period[1] == "_____" && a$PS_code[1] == '_____' && a$NM_code[1] == '_____') ||
                     (a$period[1] == "_____" && a$PS_code[1] == '_____' && a$NM_code[1] == '_____') ||
                     (a$period[1] == "_____" && a$PS_code[1] == '_____' && a$NM_code[1] == '_____')) {
            a1 <- rbind(a1, a[1, ])
            a2 <- rbind(a2, a[c(2,4,3,5), ])
            if (kods %in% c("40", "50", "53") && o == "5") {ZERO_plus(a %>% slice(5))}
          } else {stop("processingFives_s2e3: Trūkst apstrādes koda.")}
  } else if (all(a$sak_beidz[c(1,3)] == "1")) {
    #Jo pirmoreiz, bet šis ir labākais minējums, jo te varētu būt arī savādāk. (bija dziļāka izpēte)
          if (a$period[1] == "_____" && a$PS_code[1] == '_________' && a$NM_code[1] == '_____') {
            a2 <- rbind(a2, a[c(1,2,3,5), ])
      if (kods %in% c("40", "50", "53") && o == "5") {ZERO_plus(a %>% slice(5))}
    } else {stop("processingFives_s2e3: Trūkst apstrādes koda.")}
  } else {stop("processingFives_s2e3: Trūkst apstrādes koda.")}
  
  rm(a, kods)
  return(list(x5s2_uzVieniniekiem = a1,
              x5s2_uzDivniekiem = a2,
              x5s2_uzCetriniekiem = a4))
}

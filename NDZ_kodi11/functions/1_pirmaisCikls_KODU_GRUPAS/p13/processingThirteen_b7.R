processingThirteen_b7 <- function(a, o, kods) {
  
  a1 <- data.frame(); a2 <- data.frame(); a3 <- data.frame(); a4 <- data.frame()
  a8 <- data.frame(); a10 <- data.frame(); a11 <- data.frame()
  #a <- x13
  
 if (all(a$sak_beidz[c(2, 4, 6, 8, 9, 12, 13)] == "1") && 
          all(a$sak_beidz[c(1, 3, 5, 7, 10, 11)] == "2") && 
          all(sapply(seq(1, 8, by = 2), function(i) all(diff(a$NDZ_sanemsanas_datums[i:i+1]) == 0))) &&
          all(diff(a$NDZ_sanemsanas_datums[11:12]) == 0) &&
          a$NDZ_sanemsanas_datums[12] != a$NDZ_sanemsanas_datums[13]) {
              a1 <- a[13, ]; a4 <- a[1:4, ]; a8 <- a[5:12, ]
              if (kods %in% c("40", "50", "53") && o == "13") {ZERO_minus(x13 %>% slice(1))}
      } else if (((a$sak_beidz[2] == "2" && a$sak_beidz[3] == "1") && (a$NDZ_sanemsanas_datums[2] == a$NDZ_sanemsanas_datums[3])) || (a$sak_beidz[2] == "1" && a$sak_beidz[3] == "2")) {
              a3 <- a[1:3, ]; a10 <- a[4:13, ]   
      } else {stop("13-nieku tabulas pārdalei trūkst izstrādes koda. Rindas:",r, "līdz", r + 12, "\n")}
  
  rm(a, o, kods)
  return(list(x13_uzVieniniekiem = a1, 
              x13_uzDivi = a2,
              x13_uzTrijniekiem = a3,
              x13_uzCetriniekiem = a4,
              x13_uzAstoniekiem = a8, 
              x13_uzDesmitniekiem = a10,
              x13_uzVienpadsmit = a11))
}

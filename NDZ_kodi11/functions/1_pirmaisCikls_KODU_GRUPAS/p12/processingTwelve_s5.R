processingTwelve_s5 <- function(a, o, kods) {
  
  a1 <- data.frame(); a2 <- data.frame(); a7 <- data.frame(); a10 <- data.frame(); a11 <- data.frame()
  
  if(all(x12$sak_beidz[c(1, 3, 5, 7, 10)] == "1") && all(sapply(c(1:7,9,10), function(i) diff(x12$NDZ_sanemsanas_datums[i:(i+1)]) != 0)) &&
     all(sapply(c(8,11), function(i) diff(x12$NDZ_sanemsanas_datums[i:(i+1)]) == 0))) {
     a10 <- rbind(a10, x12[-c(8,11), ])
  } else {stop("processingTwelve_s5: Tabulas pārdalei trūkst izstrādes koda. Rindas: ",r, " līdz ", r + 11)}

    rm(a, o, kods)
    
    return(list(x12_uzVieniniekiem = a1,
                x12_uzDivniekiem = a2,
                x12_uzSeptini = a7,
                x12_uzDesmitniekiem = a10,
                x12_uzVienpadsmit = a11))
}

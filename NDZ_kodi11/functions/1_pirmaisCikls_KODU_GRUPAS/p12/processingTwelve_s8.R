processingTwelve_s8 <- function(a, o, kods) {
  
  a1 <- data.frame(); a2 <- data.frame(); a7 <- data.frame(); a8 <- data.frame(); a10 <- data.frame(); a11 <- data.frame()
  #a <- x12
  
  if (all(a$sak_beidz[1:4] == c("2", "1", "1", "2"))) {
    if (all(diff(a$NDZ_sanemsanas_datums[1:4]) != 0)) {
      a1 <- a[1, ]; a10 <- a[3:12, ]
    } else {stop("processingTwelve_s8 trūkst izstrādes koda.")} 
  } else {stop("processingTwelve_s8 trūkst izstrādes koda.")} 
  
  if(a$sak_beidz[12] == "2" && diff(a$NDZ_sanemsanas_datums[11:12]) != 0 &&
     kods %in% c("40", "50", "53") && o == "12") {ZERO_plus(a %>% slice(12))}
  
  rm(a, o, kods)
  
  return(list(x12_uzVieniniekiem = a1,
              x12_uzDivniekiem = a2,
              x12_uzSeptini = a7,
              x12_uzAstoni = a8,
              x12_uzDesmitniekiem = a10,
              x12_uzVienpadsmit = a11))
}

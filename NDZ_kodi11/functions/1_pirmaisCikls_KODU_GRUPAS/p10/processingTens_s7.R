processingTens_s7 <- function(x10s7, o, kods) {
  
  x10s7 <- arrange(x10s7, PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
  #x10s7 <- x10
  x10s7_uzVieniniekiem <- data.frame(); x10s7_uzDivniekiem <- data.frame(); x10s7_uzSeptini <- data.frame(); x10s7_uzAstoniekiem <- data.frame()
  
  if (all(x10s7$sak_beidz[1:5] == c("1", "2", "1", "1", "2"))) {
    if (all(diff(x10s7$NDZ_sanemsanas_datums[1:5]) != 0)) {
    x10s7_uzDivniekiem <- x10s7[1:2, ]; x10s7_uzSeptini <- x10s7[4:10, ]
    if (kods %in% c("40", "50", "53") && o == "10") {ZERO_minus(x10s7 %>% slice(1))}
    } else {stop("processingTens_s7: Trūkst izstrādes koda.")}
  } else {stop("processingTens_s7: Trūkst izstrādes koda.")}
  
  rm(x10s7, o, kods)
  return(list(x10_uzVieniniekiem = x10s7_uzVieniniekiem,
              x10_uzDivniekiem = x10s7_uzDivniekiem,
              x10_uzSeptini = x10s7_uzSeptini,
              x10_uzAstoniekiem = x10s7_uzAstoniekiem))
}

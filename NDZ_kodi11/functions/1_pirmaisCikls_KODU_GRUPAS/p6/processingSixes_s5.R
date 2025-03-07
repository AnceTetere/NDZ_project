processingSixes_s5 <- function(x6s5, o, kods) {
  
  x6s5_uzVieniniekiem <- data.frame(); x6s5_uzDivniekiem <- data.frame();
  x6s5_uzCetri <- data.frame(); x6s5_uzPieciniekiem <- data.frame()
  #x6s5 <- x6
  
  if (all(x6s5$sak_beidz == c("1", "1", "1", "1", "1", "2"))) {
    if (all(diff(x6s5$NDZ_sanemsanas_datums[1:5]) != 0) && diff(x6s5$NDZ_sanemsanas_datums[5:6]) == 0) {
      if (x6s5$PS_code[1] == '_____' && x6s5$NM_code[1] == '_____') {
        x6s5_uzDivniekiem <- rbind(x6s5_uzDivniekiem, x6s5[5:6, ])
        if (kods %in% c("40", "50", "53")) {ZERO_plus(x6s5 %>% slice(6)); ZERO_minus(x6s5 %>% slice(1))}
      } else {stop("processingSixes_s5: trūkst izstrādes koda.\n")}
    } else {stop("processingSixes_s5 trūkst apstrādes koda. \n")}
  } else {stop("processingSixes_s5 trūkst apstrādes koda\n")}
  
  rm(x6s5, o, kods)
  return(list(x6_uzVieniniekiem = x6s5_uzVieniniekiem, 
              x6_uzDivniekiem = x6s5_uzDivniekiem,
              x6_uzCetri = x6s5_uzCetri,
              x6_uzPieciniekiem = x6s5_uzPieciniekiem))
}

processingEleven_s6_2121 <- function(b, o, kods) {
  b1 <- data.frame(); b2 <- data.frame(); b9 <- data.frame(); b10 <- data.frame()
  #b <- a
  
  if (all(diff(b$NDZ_sanemsanas_datums[1:4]) != 0)) {
            
            if (b$period[1] == '______' && b$PS_code[1] ==  '______________' && b$NM_code[1] ==  '______________') {
                  b1 <- b[1, ]; b10 <- b[-1,]
            } else {stop("processingEleven_s6_2121 trūkst izstrādes koda. \n")}
  } else if (all(sapply(c(1,3), function(i) diff(b$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) && 
             all(sapply(c(2,4,5), function(i) diff(b$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
            
            if (b$period[1] == '______' && b$PS_code[1] ==  '______________' && b$NM_code[1] ==  '______________') {
                b2 <- b[c(2,1), ]; b9 <- b[3:11,]
            } else {stop("processingEleven_s6_2121: Trūkst izstrādes koda. \n")}
  } else if (all(sapply(c(1,3,5), function(i) diff(b$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) && 
             all(sapply(c(2,4,6), function(i) diff(b$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
             
             if (b$period[1] == '______' && b$PS_code[1] ==  '______________' && b$NM_code[1] ==  '______________') {
                 b2 <- b[c(2,1), ]; b9 <- b[3:11,]
                 if (kods %in% c("40", "50", "53") && o == "11") {ZERO_minus(a %>% slice(2))}
             } else {stop("processingEleven_s6_2121: Trūkst izstrādes koda. \n")}
  }else {stop("processingEleven_s6_2121: Trūkst izstrādes koda. \n")}
  
  rm(b, o, kods)
  return(list(a1 = b1, a2 = b2, a9 = b9, a10 = b10))
}

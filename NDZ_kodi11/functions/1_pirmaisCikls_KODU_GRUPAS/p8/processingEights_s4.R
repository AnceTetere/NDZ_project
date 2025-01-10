processingEights_s4 <- function(x8s4) {

  x8s4 <- x8s4 %>% arrange(PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
  x8s4_uzVieniniekiem <- data.frame(); x8s4_uzDivniekiem <- data.frame(); x8s4_uzCetriniekiem <- data.frame()
  x8s4_uzSesi <- data.frame(); x8s4_uzSeptini <- data.frame()

if (x8s4$PS_code[1] == '_________' && x8s4$NM_code[1] == '_________') {
  if (all(x8s4$sak_beidz == c("2","1","2","2","1","2","1","1"))) {
    x8s4_uzVieniniekiem <- rbind(x8s4_uzVieniniekiem, x8s4[c(1,8), ])
    x8s4_uzCetriniekiem <- rbind(x8s4_uzCetriniekiem, x8s4[c(2,4,5,6),])
  } else {stop("processingEights_s4: Trūkst izstrādes koda.")}
} else if (all(sapply(seq(1,8,by=2), function(i) diff(x8s4$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
   all(sapply(seq(2,7,by=2), function(i) diff(x8s4$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
  #cS (check sak_beidz) norāda kombinācijas, kurās rindās izejas tabulas ailē sak_beidz atrodas "1".
  cS <- list(c(2, 4, 5, 8), c(2, 4, 6, 8), c(1, 4, 6, 7), c(2, 3, 6, 8), c(2, 3, 5, 7), c(2, 3, 5, 8), c(1, 4, 5, 7), c(1, 3, 6, 8), c(1, 3, 5, 7), c(2, 3, 6, 7))
  found_match <- FALSE
  
  #Salīdzina vai šajā apakštabulā kāda no cS kombinācijām atrodama.
  for (s in cS) {
    if (all(x8s4$sak_beidz[s] == "1")) {
      x8s4_uzCetriniekiem <- rbind(x8s4_uzCetriniekiem, x8s4)
      found_match <- TRUE
      break #kad atrasts, norauj cilpu  
    }}
  if (!found_match) {
    if(all(x8s4$sak_beidz[c(3,4,7,8)] == "1") && x8s4$zinkod[1] %in% c("40", "41", "90", "91")) {
      x8s4_uzVieniniekiem <- rbind(x8s4_uzVieniniekiem, x8s4[c(1, 8), ])
      x8s4_uzDivniekiem <- rbind(x8s4_uzDivniekiem, x8s4[4:5, ])
    } else {stop("processingEights_s4: Trūkst izstrādes koda.")}
    rm(cS, s, found_match)
  }
} else if (all(x8s4$sak_beidz[1:3] == c("2", "1", "2")) && 
           diff(x8s4$NDZ_sanemsanas_datums[1:2]) != 0) {
      x8s4_uzVieniniekiem <- rbind(x8s4_uzVieniniekiem, x8s4[1, ])
      x8s4_uzSeptini <- rbind(x8s4_uzSeptini, x8s4[-1, ])
} else if (all(x8s4$sak_beidz[1:3] == c("2", "2", "1")) && diff(x8s4$NDZ_sanemsanas_datums[2:3]) == 0) {
      x8s4_uzVieniniekiem <- rbind(x8s4_uzVieniniekiem, x8s4[1, ])
      x8s4_uzSeptini <- rbind(x8s4_uzSeptini, x8s4[-1, ])
} else if (all(x8s4$sak_beidz[c(1, 4, 6, 8)] == "1") && all(diff(x8s4$NDZ_sanemsanas_datums) != 0)) {
      x8s4_uzVieniniekiem <- rbind(x8s4_uzVieniniekiem, x8s4[8, ])
      x8s4_uzSesi <- rbind(x8s4_uzSesi, x8s4[c(1, 3:7), ])
} else if (all(x8s4$sak_beidz[c(1,2,4,7)] == "1") && 
           all(sapply(c(2,4), function(i) diff(x8s4$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
           all(sapply(c(1,3,5:7), function(i) diff(x8s4$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
      x8s4_uzDivniekiem <- rbind(x8s4_uzDivniekiem, x8s4[c(1,3,4,6), ])
      x8s4_uzCetriniekiem <- rbind(x8s4_uzCetriniekiem, x8s4[c(2,5,7,8), ]) 
} else {
  x8s4_uzCetriniekiem  <- rbind(x8s4_uzCetriniekiem, x8s4)
  #stop("processingEights_s4: Trūkst izstrādes koda.")
}

rm(x8s4)

return(list(x8_uzVieniniekiem = x8s4_uzVieniniekiem, 
            x8_uzDivniekiem = x8s4_uzDivniekiem, 
            x8_uzCetriniekiem = x8s4_uzCetriniekiem, 
            x8_uzSesi = x8s4_uzSesi, 
            x8_uzSeptini = x8s4_uzSeptini))
}

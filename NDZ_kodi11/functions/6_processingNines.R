
#1 Tabulu, kurā unikālais indivīds - definēts kā period == ps_code == dn_code == nm_code - parādās 9 reizes,
# pārdali xxxxxxxx un sūti caur xxxx.

x <- x[order(x$period, x$ps_code, x$dn_code, x$nm_code, x$NDZ_sanemsanas_datums), ]
x9_uzVieniniekiem <- data.frame()
x9_uzCetriniekiem <- data.frame()


for (r in seq(1, nrow(x), by = 9)) {
  #TESTĒŠANAI r <- sample(1: nrow(x), size = 1, replace = FALSE)
  x9 <- x[x$ps_code == x$ps_code[r], ]
  # TESTĒŠANAI: x[x$ps_code == x$ps_code[r], ]
  
  if (((x9$start[1] == "1" && x9$start[3] == "1") && ((x9$start[5] == "1" && x9$start[7] == "1") && (x9$start[9] == "1" && x9$end[2] == "2")))&&((x9$end[4] == "2" && x9$end[6] == "2") && x9$end[8] == "2")) {
    x9_uzVieniniekiem <- rbind(x9_uzVieniniekiem, x9[9, ])
    x9_uzCetriniekiem <- rbind(x9_uzCetriniekiem, x9[1:8, ])
  } else if((((sum(x9$start == "1") == 4 &&sum(x9$end == "2") == 5) && (x9$end[1] == "2" && x9$NDZ_sanemsanas_datums[1] != x9$NDZ_sanemsanas_datums[2])) && ((((x9$start[2] == "1" && x9$end[3] == "2") && x9$NDZ_sanemsanas_datums[2] <= x9$NDZ_sanemsanas_datums[3])||((x9$end[2] == "2" && x9$start[3] == "1") && x9$NDZ_sanemsanas_datums[2] == x9$NDZ_sanemsanas_datums[3])) && (((x9$start[4] == "1" && x9$end[5] == "2") && x9$NDZ_sanemsanas_datums[4] <= x9$NDZ_sanemsanas_datums[5])||((x9$end[4] == "2" && x9$start[5] == "1") && x9$NDZ_sanemsanas_datums[4] == x9$NDZ_sanemsanas_datums[5])))) && ((((x9$start[6] == "1" && x9$end[7] == "2") && x9$NDZ_sanemsanas_datums[6] <= x9$NDZ_sanemsanas_datums[7])||((x9$end[6] == "2" && x9$start[7] == "1") && x9$NDZ_sanemsanas_datums[6] == x9$NDZ_sanemsanas_datums[7])) && (((x9$start[8] == "1" && x9$end[9] == "2") && x9$NDZ_sanemsanas_datums[8] <= x9$NDZ_sanemsanas_datums[9])||((x9$end[8] == "2" && x9$start[9] == "1") && x9$NDZ_sanemsanas_datums[8] == x9$NDZ_sanemsanas_datums[9])))){
    x9_uzVieniniekiem <- rbind(x9_uzVieniniekiem, x9[1, ])
    x9_uzCetriniekiem <- rbind(x9_uzCetriniekiem, x9[2:9, ])
  } else {
    stop(cat("Deviņnieku izstrādē processingNines() gadījums, kas atrodams izejas tabulas x rindās",  r, "līdz", r+8, "nav izstrādāts!"))
  }
}

#2 PĀRBAUDE: Vai rindu skaits no deviņniekiem atvasinātajās tabulās sakrīt ar rindām izejas tabulā x.
if ((nrow(x9_uzCetriniekiem) + nrow(x9_uzVieniniekiem)) == nrow(x)) {
  cat("PĀRBAUDE IZIETA: Apakštabulu rindu summa sakrīt ar izejošo devītnieku tabulu.")
  rm(x, x9)
} else {
  stop(cat("PĀRBAUDE NAV IZIETA: Apakštabulu rindu summa NESAKRĪT ar izejošo devītnieku tabulu."))
}

#3 Apakštabulu x9_uzVieniniekiem sūta caur processingOnes().
if(nrow(x9_uzVieniniekiem) > 0) {
  x9_uzVieniniekiem <- x9_uzVieniniekiem[order(x9_uzVieniniekiem$ps_code, x9_uzVieniniekiem$nm_code, x9_uzVieniniekiem$NDZ_sanemsanas_datums), ]
  cat("No devītniekiem atvasinātā tabula x9_uzVieniniekiem pārsūtīta uz processingOnes un tad uz tempNDZ, ko būvējam.")
  sendTo_tempNDZ(processingOnes(x9_uzVieniniekiem, o))
} else {
  cat("Tabula x9_uzVieniniekiem ir tukša.")
}
rm(x9_uzVieniniekiem)

#4 Apakštabulu x9_uzCetriniekiem sūta caur precessingFours().
if(nrow(x9_uzCetriniekiem) > 0) {
  x9_uzCetriniekiem <- x9_uzCetriniekiem[order(x9_uzCetriniekiem$ps_code, x9_uzCetriniekiem$nm_code, x9_uzCetriniekiem$NDZ_sanemsanas_datums), ]
  cat("No devītniekiem atvasinātā tabula x9_uzCetriniekiem pārsūtīta uz processingFours un tad uz tempNDZ, ko būvējam.")
  processingFours(x9_uzCetriniekiem, o)
} else {
  cat("Tabula x9_uzCetriniekiem ir tukša.")
}
rm(x9_uzCetriniekiem) 

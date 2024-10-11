processingFours <- function(x, o) {
  x <- arrange(x, PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)

  x4_uzVieniniekiem <- data.frame()
  x4_trueDoubles <- data.frame()
  x4_uzTrijniekiem <- data.frame()
  check_rows <- 0

for (r in seq(1, nrow(x), by = 4)) {

  x4 <- x[r:(r + 3), ]
  x4 <- arrange(x4, PS_code, DN_code, NM_code, NDZ_sanemsanas_datums)
  
  if(!(doublesTest(1, x4) && doublesTest(3, x4))) {
    stop("Četrinieku apstrādes tabulā, ko izstrādā caur funkciju processingFours(), 
               rindās no", r, "līdz", r + 3, "nesakrīt pseidokoda, NM_code, DN_code, period kombinācija visās četrās rindās.")
  } else if (all(x4$sak_beidz == c("2", "1", "2", "2"))) {
    if(diff(x4$NDZ_sanemsanas_datums[1:2]) > 0 && diff(x4$NDZ_sanemsanas_datums[2:3]) == 0 && x4$zinkod[1] %in% c("40", "41", "50", "51", "53", "54", "91", "92")) {
      x4_uzVieniniekiem <- rbind(x4_uzVieniniekiem, x4[1, ])
      x4_trueDoubles <- rbind(x4_trueDoubles, x4[2:3, ])
    } else if (diff(x4$NDZ_sanemsanas_datums[1:2]) == 0 && all(diff(x4$NDZ_sanemsanas_datums[2:4]) > 0) &&
               x4$period[1] == "202101" && ((x4$PS_code[1] == "____________" && x4$NM_code[1] == "________") ||(x4$PS_code[1] == "________" && x4$NM_code[1] == "_________________"))) {
      # Lēmums: ..\manual\lēmumi\kods50_1222
      x4_uzVieniniekiem <- rbind(x4_uzVieniniekiem, x4[2, ])
    } else if (all(diff(x4$NDZ_sanemsanas_datums) > 0)) {
      x4_uzVieniniekiem <- rbind(x4_uzVieniniekiem, x4[1, ])
      x4_trueDoubles <- rbind(x4_trueDoubles, x4[c(2,4), ])
    } else if (diff(x4$NDZ_sanemsanas_datums[1:2]) == 0 && all(diff(x4$NDZ_sanemsanas_datums[2:4]) > 0) &&
               ((x4$PS_code[1] == "___________" && x4$NM_code[1] == "___________") || (x4$PS_code[1] == "__________" && x4$NM_code[1] == "_____________"))) {
      # Lēmums: ..\manual\lēmumi\kods50_1222
      x4_trueDoubles <- rbind(x4_trueDoubles, x4[c(2, 4), ])
    } else {stop("Šeit četrinieku izstrādes tabulas rindām ", r, " līdz ", r+3, " trūkst apstrādes koda.")}
  } else if (all(x4$sak_beidz == c("1", "2", "1", "2"))) {
    x4_trueDoubles <- rbind(x4_trueDoubles, x4)
  } else if (all(x4$sak_beidz == c("1", "1", "2", "1"))) {
    if (diff(x4$NDZ_sanemsanas_datums[3:4]) > 0) {
      x4_uzVieniniekiem <- rbind(x4_uzVieniniekiem, x4[4, ]) 
      x4_uzTrijniekiem <- rbind(x4_uzTrijniekiem, x4[1:3, ])
    } else if (diff(x4$NDZ_sanemsanas_datums[2:3]) >= 0) {
      x4_uzTrijniekiem <- rbind(x4_uzTrijniekiem, x4[2:4, ])
    } else if (all(diff(x4$NDZ_sanemsanas_datums[1:3]) != 0) && diff(x4$NDZ_sanemsanas_datums[3:4]) == 0) {
      x4_trueDoubles <- rbind(x4_trueDoubles, x4[3:4, ])
    } else {stop("Šeit četrinieku izstrādes tabulas rindām ", r, " līdz ", r+3, " trūkst apstrādes koda.")}
  } else if (all(x4$sak_beidz == c("2", "1", "2", "1"))) {
    if (diff(x4$NDZ_sanemsanas_datums[1:2]) > 0) {
      x4_uzVieniniekiem <- rbind(x4_uzVieniniekiem, x4[1, ])
      x4_uzTrijniekiem <- rbind(x4_uzTrijniekiem, x4[2:4, ])
    } else if (all(sapply(c(1,3), function(i) diff(x4$NDZ_sanemsanas_datums[i:(i+1)]) == 0))) {
      x4 <- arrange(x4, NDZ_sanemsanas_datums, sak_beidz) 
      x4_trueDoubles <- rbind(x4_trueDoubles, x4)
    } else if (diff(x4$NDZ_sanemsanas_datums[1:2]) == 0 && all(diff(x4$NDZ_sanemsanas_datums[2:4]) > 0)) {
      x4_trueDoubles <- rbind(x4_trueDoubles, x4[c(2,1,3,4),])
    } else {stop("Šeit četrinieku izstrādes tabulas rindām ", r, " līdz ", r+3, " trūkst apstrādes koda.")}
  } else if (all(x4$sak_beidz == c("1", "2", "1", "1"))) {
    if (diff(x4$NDZ_sanemsanas_datums[1:2]) >= 0) {
      x4_trueDoubles <- rbind(x4_trueDoubles, x4[1:2, ])
      x4_uzVieniniekiem <- rbind(x4_uzVieniniekiem, x4[4, ])
    } else {stop("Šeit četrinieku izstrādes tabulas rindām ", r, " līdz ", r+3, " trūkst apstrādes koda.")}
  } else if (all(x4$sak_beidz == "1") || all(x4$sak_beidz == "2")) {
    x4_uzVieniniekiem <- rbind(x4_uzVieniniekiem, codes_match(x4))
  } else if (all(x4$sak_beidz == c("1", "1", "1", "2"))) {
    x4_trueDoubles <- rbind(x4_trueDoubles, x4[3:4, ])
  } else if (all(x4$sak_beidz == c("1", "2", "2", "1"))) {
    if (diff(x4$NDZ_sanemsanas_datums[1:2]) >= 0 && diff(x4$NDZ_sanemsanas_datums[3:4]) > 0) {
      x4_uzVieniniekiem <- rbind(x4_uzVieniniekiem, x4[4, ])
      x4_trueDoubles <- rbind(x4_trueDoubles, x4[c(1, 3), ])
    } else if (diff(x4$NDZ_sanemsanas_datums[1:2]) >= 0 && diff(x4$NDZ_sanemsanas_datums[2:3]) > 0 && diff(x4$NDZ_sanemsanas_datums[3:4]) == 0) {
      x4 <- arrange(x4, NDZ_sanemsanas_datums, sak_beidz) 
      x4_trueDoubles <- rbind(x4_trueDoubles, x4)
    } else {stop("Šeit četrinieku izstrādes tabulas rindām ", r, " līdz ", r+3, " trūkst apstrādes koda.")}
  } else if (all(x4$sak_beidz == c("2", "1", "1", "1"))) {
    if (diff(x4$NDZ_sanemsanas_datums[1:2]) > 0 && diff(x4$NDZ_sanemsanas_datums[2:3]) > 0) {
      x4_uzVieniniekiem <- rbind(x4_uzVieniniekiem, x4[c(1, 4), ])
    } else if (diff(x4$NDZ_sanemsanas_datums[1:2]) == 0 && diff(x4$NDZ_sanemsanas_datums[2:3]) > 0) {
      x4_uzVieniniekiem <- rbind(x4_uzVieniniekiem, x4[4, ])
      x4_trueDoubles <- rbind(x4_trueDoubles, x4[c(2,1), ])
    } else {stop("Šeit četrinieku izstrādes tabulas rindām ", r, " līdz ", r+3, " trūkst apstrādes koda.")}
  } else if (all(x4$sak_beidz == c("2", "2", "1", "1"))) {
    if (diff(x4$NDZ_sanemsanas_datums[2:3]) > 0) {
      x4_uzVieniniekiem <- rbind(x4_uzVieniniekiem, x4[c(2, 4), ])
    } else if (all(sapply(c(1,3), function(i) diff(x4$NDZ_sanemsanas_datums[i:(i+1)]) > 0)) &&
               diff(x4$NDZ_sanemsanas_datums[2:3]) == 0) {
      x4 <- arrange(x4, NDZ_sanemsanas_datums, sak_beidz) 
      x4_uzVieniniekiem <- rbind(x4_uzVieniniekiem, x4[c(1,4), ])
      x4_trueDoubles <- rbind(x4_trueDoubles, x4[2:3,])
    } else {stop("Šeit četrinieku izstrādes tabulas rindām ", r, " līdz ", r+3, " trūkst apstrādes koda.")}
  } else if (all(x4$sak_beidz == c("2", "1", "2", "2"))) {
    if (diff(x4$NDZ_sanemsanas_datums[1:2]) > 0){
      x4_uzVieniniekiem <- rbind(x4_uzVieniniekiem, x4[1, ])
      x4_trueDoubles <- rbind(x4_trueDoubles, x4[c(2, 4), ])
    } else {stop("Šeit četrinieku izstrādes tabulas rindām ", r, " līdz ", r+3, " trūkst apstrādes koda.")}
  } else if (all(x4$sak_beidz == c("2", "2", "1", "2"))) {
    if (diff(x4$NDZ_sanemsanas_datums[1:2]) > 0 && diff(x4$NDZ_sanemsanas_datums[2:3]) == 0) {
      x4_uzVieniniekiem <- rbind(x4_uzVieniniekiem, x4[1, ])
      x4_trueDoubles <- rbind(x4_trueDoubles, x4[c(2, 4), ])
    } else if (all(diff(x4$NDZ_sanemsanas_datums) > 0)) {
      x4_uzVieniniekiem <- rbind(x4_uzVieniniekiem, x4[2, ])
      x4_trueDoubles <- rbind(x4_trueDoubles, x4[3:4, ])
    } else {stop("Šeit četrinieku izstrādes tabulas rindām ", r, " līdz ", r+3, " trūkst apstrādes koda.")}
  } else if (all(x4$sak_beidz == c("1", "2", "2", "2"))) {
    if ("26" %in% x4$zinkod) {
      x4_trueDoubles <- rbind(x4_trueDoubles, x4[x4$sak_beidz == "1" | x4$zinkod == "26", ])
    } else if(all(diff(x4$NDZ_sanemsanas_datums) != 0)){
      x4_trueDoubles <- rbind(x4_trueDoubles, x4[c(1,4), ])
    } else {stop("Šeit četrinieku izstrādes tabulas rindām ", r, " līdz ", r+3, " trūkst apstrādes koda.")}
  } else if (all(x4$sak_beidz == c("2", "1", "1", "2"))) { 
    if (diff(x4$NDZ_sanemsanas_datums[1:2]) > 0) {
      x4_uzVieniniekiem <- rbind(x4_uzVieniniekiem, x4[1, ])
      x4_trueDoubles <- rbind(x4_trueDoubles, x4[3:4, ])
    } else if (diff(x4$NDZ_sanemsanas_datums[1:2]) == 0 && all(diff(x4$NDZ_sanemsanas_datums[2:4]) != 0)){
      x4_trueDoubles <- rbind(x4_trueDoubles, x4[c(2,1,3,4), ])
    } else {stop("Šeit četrinieku izstrādes tabulas rindām ", r, " līdz ", r+3, " trūkst apstrādes koda.")}
  } else if(all(x4$sak_beidz == c("1", "1", "2", "2"))) { 
    if (diff(x4$NDZ_sanemsanas_datums[2:3]) == 0 && all(sapply(c(1,3), function(i) diff(x4$NDZ_sanemsanas_datums[i:(i+1)]) != 0)) && x4$PS_code[1] == '________' && x4$NM_code[1] == '____________') {
      x4_trueDoubles <- rbind(x4_trueDoubles, x4[c(1,3,2,4), ])
    } else if (all(diff(x4$NDZ_sanemsanas_datums) > 0)) {
      x4_trueDoubles <- rbind(x4_trueDoubles, x4[c(2,4), ])
    } else {stop("Šeit četrinieku izstrādes tabulas rindām ", r, " līdz ", r+3, " trūkst apstrādes koda.")}
  } else {
    stop(cat("Šeit četrinieku izstrādes tabulas rindām", r, "līdz", r+3, "trūkst apstrādes koda."))
  }
  check_rows <- check_rows + 4
}


#PĀRBAUDE
  if(nrow(x) == check_rows) {
    cat("PĀRBAUDE IZIETA: Četrinieku tabula veiksmīgi pārdalījusies apakštabulās.\n"); rm(x, check_rows, r, x4)
  } else {stop("processingFours 
                     Četrinieku tabula NAV pārdalījusies.
                     check_rows cipars nesakrīt ar rindu skaitu izejas tabulā.\n")}

#1 Atvasināto tabulu x4_uzVieniniekiem sūta uz vieninieku apstrādi
if(nrow(x4_uzVieniniekiem) > 0) {
  sendTo_tempNDZ(processingOnes(x4_uzVieniniekiem, o))
} else {cat("No četriniekiem pārsūtāmajā vieninieku tabulā nebija nevienas rindas.\n")}
rm(x4_uzVieniniekiem)
    
#2 Atvasināto tabulu x4_trueDoubles sūta caur processingTwoes().
if(nrow(x4_trueDoubles) > 0) {
  processingTwoes(x4_trueDoubles, o)
  cat("No četrinieku apstrādes tabula pārsūtīta uz processingTwoes().\n")
} else {cat("No četriniekiem pārsūtāmajā divnieku tabulā nebija nevienas rindas.\n")}
rm(x4_trueDoubles)

#3 Atvasināto tabulu x4_uzTrijniekiem sūta caur processingThrees().
if(nrow(x4_uzTrijniekiem) > 0) {
  processingThrees(x4_uzTrijniekiem, o)
} else {cat("No četriniekiem pārsūtāmajā trijnieku tabulā nebija nevienas rindas.\n")}
rm(x4_uzTrijniekiem)
}


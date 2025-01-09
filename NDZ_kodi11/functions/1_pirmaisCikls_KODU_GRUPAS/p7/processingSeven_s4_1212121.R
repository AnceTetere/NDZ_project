processingSeven_s4_1212121 <- function(x7s4a) {
  
  x7s4a_uzVieniniekiem <- data.frame()
  x7s4a_uzSesiniekiem <- data.frame()
  
  if (all(diff(x7s4a$NDZ_sanemsanas_datums) != 0)) {
    x7s4a_uzSesiniekiem <- rbind(x7s4a_uzSesiniekiem, x7s4a[1:6, ])
    x7s4a_uzVieniniekiem <- rbind(x7s4a_uzVieniniekiem, x7s4a[7, ])
  } else if (all(sapply(c(1,3), function(i) diff(x7s4a$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
             all(sapply(c(2,4,5,6), function(i) diff(x7s4a$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
    x7s4a_uzSesiniekiem <- rbind(x7s4a_uzSesiniekiem, x7s4a[1:6, ])
    x7s4a_uzVieniniekiem <- rbind(x7s4a_uzVieniniekiem, x7s4a[7, ])
  } else if (diff(x7s4a$NDZ_sanemsanas_datums[3:4]) == 0 &&
             all(sapply(c(1,2,4,5,6), function(i) diff(x7s4a$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
    x7s4a_uzSesiniekiem <- rbind(x7s4a_uzSesiniekiem, x7s4a[1:6, ])
    x7s4a_uzVieniniekiem <- rbind(x7s4a_uzVieniniekiem, x7s4a[7, ])
  } else if (all(sapply(c(3,5), function(i) diff(x7s4a$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
             all(sapply(c(1,2,4,6), function(i) diff(x7s4a$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
    x7s4a_uzSesiniekiem <- rbind(x7s4a_uzSesiniekiem, x7s4a[1:6, ])
    x7s4a_uzVieniniekiem <- rbind(x7s4a_uzVieniniekiem, x7s4a[7, ])
  } else if (all(sapply(c(2,4,6), function(i) diff(x7s4a$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
             all(sapply(c(1,3,5), function(i) diff(x7s4a$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
    if (x7s4a$period[1] == "________" && x7s4a$PS_code[1] == "________" && x7s4a$NM_code[1] == "________") {
      x7s4a_uzSesiniekiem <- rbind(x7s4a_uzSesiniekiem, x7s4a[c(3,2,5,4,7,6), ])
    } else {stop("processingSeven: Septiņnieku apstrādē, gadījumam rindās ", r, " līdz ", r + 6, " trūkst izstrādes koda.\n")}
  } else {stop("processingSeven: Septiņnieku apstrādē, gadījumam rindās ", r, " līdz ", r + 6, " trūkst izstrādes koda.\n")}

   
  return(list(x7s4_uzVieniniekiem = x7s4a_uzVieniniekiem,
              x7s4_uzSesiniekiem  = x7s4a_uzSesiniekiem))
}

processingEights_s4 <- function(x8s4) {
x8s4 <- x8s4[order(x8s4$PS_code, x8s4$DN_code, x8s4$NM_code, x8s4$NDZ_sanemsanas_datums), ]

x8s4_4_uzVieniniekiem <- data.frame()
x8s4_4_uzCetriniekiem <- data.frame()
x8s4_4_uzSesi <- data.frame()
x8s4_4_uzSeptini <- data.frame()


if (all(x8s4$start[c(1, 3, 5, 7)] == "1") && x8s4$NDZ_sanemsanas_datums[1] <= x8s4$NDZ_sanemsanas_datums[2]) {
  x8s4_4_uzCetriniekiem  <- rbind(x8s4_4_uzCetriniekiem , x8s4)
} else if (x8s4$end[1] == "2" && x8s4$start[2] == "1" && x8s4$end[3] == "2" && x8s4$NDZ_sanemsanas_datums[1] != x8s4$NDZ_sanemsanas_datums[2]) {
  x8s4_4_uzVieniniekiem <- rbind(x8s4_4_uzVieniniekiem, x8s4[1, ])
  x8s4_4_uzSeptini <- rbind(x8s4_4_uzSeptini, x8s4[-1, ])
} else if (x8s4$end[1] == "2" && x8s4$end[2] == "2" && x8s4$start[3] == "1" && x8s4$NDZ_sanemsanas_datums[2] == x8s4$NDZ_sanemsanas_datums[3]) {
  x8s4_4_uzVieniniekiem <- rbind(x8s4_4_uzVieniniekiem, x8s4[1, ])
  x8s4_4_uzSeptini <- rbind(x8s4_4_uzSeptini, x8s4[-1, ])
} else if (all(x8s4$start[c(1, 4, 6, 8)] == "1") && all(diff(x8s4$NDZ_sanemsanas_datums) != 0)) {
  x8s4_4_uzVieniniekiem <- rbind(x8s4_4_uzVieniniekiem, x8s4[8, ])
  x8s4_4_uzSesi <- rbind(x8s4_4_uzSesi, x8s4[c(1, 3:7), ])
} else if (all(x8s4$start[c(2, 4, 6, 7)] == "1") && 
           all(sapply(seq(1, 6, by = 2), function(i) diff(x8s4$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
           all(sapply(c(2,4,6,7), function(i) diff(x8s4$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
  x8s4_4_uzCetriniekiem <- rbind(x8s4_4_uzCetriniekiem, x8s4)
} else if (all(x8s4$start[c(2, 4, 6, 8)] == "1") && 
           all(sapply(seq(1, 8, by = 2), function(i) diff(x8s4$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
           all(sapply(seq(2,7, by = 2), function(i) diff(x8s4$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
  x8s4_4_uzCetriniekiem <- rbind(x8s4_4_uzCetriniekiem, x8s4) 
} else if (all(x8s4$start[c(2, 3, 5, 7)] == "1") && 
           all(sapply(c(1,7), function(i) diff(x8s4$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
           all(sapply(2:6, function(i) diff(x8s4$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
  x8s4_4_uzCetriniekiem <- rbind(x8s4_4_uzCetriniekiem, x8s4) 
} else if (all(x8s4$start[c(1, 4, 5, 7)] == "1") && 
           all(sapply(seq(3,8, by=2), function(i) diff(x8s4$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
           all(sapply(c(1,2,4,6), function(i) diff(x8s4$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
  x8s4_4_uzCetriniekiem <- rbind(x8s4_4_uzCetriniekiem, x8s4) 
} else if (all(x8s4$start[c(1, 3, 6, 8)] == "1") && 
           all(sapply(seq(5,8, by=2), function(i) diff(x8s4$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
           all(sapply(1:4, function(i) diff(x8s4$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
  x8s4_4_uzCetriniekiem <- rbind(x8s4_4_uzCetriniekiem, x8s4) 
} else if (all(x8s4$start[c(2, 3, 5, 8)] == "1") && 
           all(sapply(c(1,3,7), function(i) diff(x8s4$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
           all(sapply(c(2,4,5,6), function(i) diff(x8s4$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
  x8s4_4_uzCetriniekiem <- rbind(x8s4_4_uzCetriniekiem, x8s4) 
} else if (all(x8s4$start[c(2, 3, 6, 8)] == "1") && 
           all(sapply(seq(1,8, by=2), function(i) diff(x8s4$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
           all(sapply(seq(2,7, by=2), function(i) diff(x8s4$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
  x8s4_4_uzCetriniekiem <- rbind(x8s4_4_uzCetriniekiem, x8s4) 
} else if (all(x8s4$start[c(2, 3, 5, 7)] == "1") && 
           all(sapply(seq(1,8, by=2), function(i) diff(x8s4$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
           all(sapply(seq(2,7, by=2), function(i) diff(x8s4$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
  x8s4_4_uzCetriniekiem <- rbind(x8s4_4_uzCetriniekiem, x8s4) 
} else if (all(x8s4$start[c(1, 3, 6, 7)] == "1") && 
           all(sapply(seq(3,8, by=2), function(i) diff(x8s4$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
           all(sapply(c(1,2,4,6), function(i) diff(x8s4$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
  x8s4_4_uzCetriniekiem <- rbind(x8s4_4_uzCetriniekiem, x8s4) 
} else if (all(x8s4$start[c(1, 3, 6, 7)] == "1") && 
           diff(x8s4$NDZ_sanemsanas_datums[5:6]) == 0 &&
           all(sapply(c(1,2,3,4,6), function(i) diff(x8s4$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
  x8s4_4_uzCetriniekiem <- rbind(x8s4_4_uzCetriniekiem, x8s4) 
} else if (all(x8s4$start[c(1, 3, 5, 8)] == "1") && 
           diff(x8s4$NDZ_sanemsanas_datums[7:8]) == 0 &&
           all(sapply(1:6, function(i) diff(x8s4$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
  x8s4_4_uzCetriniekiem <- rbind(x8s4_4_uzCetriniekiem, x8s4) 
} else if (all(x8s4$start[c(2, 3, 5, 8)] == "1") && 
           all(sapply(seq(1,8, by=2), function(i) diff(x8s4$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
           all(sapply(seq(2,7, by=2), function(i) diff(x8s4$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
  x8s4_4_uzCetriniekiem <- rbind(x8s4_4_uzCetriniekiem, x8s4) 
} else if (all(x8s4$start[c(1, 4, 5, 8)] == "1") && 
           all(sapply(seq(3,8, by=2), function(i) diff(x8s4$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
           all(sapply(c(1,2,4,6), function(i) diff(x8s4$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
  x8s4_4_uzCetriniekiem <- rbind(x8s4_4_uzCetriniekiem, x8s4) 
} else if (all(x8s4$start[c(2, 3, 6, 7)] == "1") && 
           all(sapply(c(1,5), function(i) diff(x8s4$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
           all(sapply(c(2,3,4,6,7), function(i) diff(x8s4$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
  x8s4_4_uzCetriniekiem <- rbind(x8s4_4_uzCetriniekiem, x8s4) 
} else if (all(x8s4$start[c(1, 4, 5, 7)] == "1") && 
           all(sapply(c(3,7), function(i) diff(x8s4$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
           all(sapply(c(1,2,4,5,6), function(i) diff(x8s4$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
  x8s4_4_uzCetriniekiem <- rbind(x8s4_4_uzCetriniekiem, x8s4) 
} else if (all(x8s4$start[c(2, 3, 5, 7)] == "1") && 
           diff(x8s4$NDZ_sanemsanas_datums[1:2]) == 0 &&
           all(diff(x8s4$NDZ_sanemsanas_datums[2:8]) != 0)) {
  x8s4_4_uzCetriniekiem <- rbind(x8s4_4_uzCetriniekiem, x8s4) 
} else if (all(x8s4$start[c(1, 4, 5, 7)] == "1") && 
           diff(x8s4$NDZ_sanemsanas_datums[3:4]) == 0 &&
           all(sapply(c(1,2,4:7), function(i) diff(x8s4$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
  x8s4_4_uzCetriniekiem <- rbind(x8s4_4_uzCetriniekiem, x8s4) 
} else if (all(x8s4$start[c(1, 3, 6, 8)] == "1") && 
           all(sapply(seq(1,8,by=2), function(i) diff(x8s4$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
           all(sapply(seq(2,7,by=2), function(i) diff(x8s4$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
  x8s4_4_uzCetriniekiem <- rbind(x8s4_4_uzCetriniekiem, x8s4) 
} else if (all(x8s4$start[c(1, 3, 5, 8)] == "1") && 
           all(sapply(c(1,7), function(i) diff(x8s4$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
           all(sapply(2:6, function(i) diff(x8s4$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
  x8s4_4_uzCetriniekiem <- rbind(x8s4_4_uzCetriniekiem, x8s4) 
} else if (all(x8s4$start[c(1, 3, 5, 8)] == "1") && 
           all(sapply(c(1,3, 7), function(i) diff(x8s4$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
           all(sapply(c(2,4,5,6), function(i) diff(x8s4$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
  x8s4_4_uzCetriniekiem <- rbind(x8s4_4_uzCetriniekiem, x8s4) 
} else {stop("processingEights_4: Trūkst izstrādes koda rindās ", r, " līdz ", r + 7,".")}

rm(x8s4)
return(list(x8s4_4_uzVieniniekiem = x8s4_4_uzVieniniekiem, x8s4_4_uzCetriniekiem = x8s4_4_uzCetriniekiem, x8s4_4_uzSesi = x8s4_4_uzSesi, x8s4_4_uzSeptini = x8s4_4_uzSeptini))
}

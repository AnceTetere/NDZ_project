processingFives_s3 <- function(x5s3) {
  x5s3 <- x5s3[order(x5s3$PS_code, x5s3$DN_code, x5s3$NM_code, x5s3$NDZ_sanemsanas_datums), ]
  #x5s3 <- x5
  x5s3_uzVieniniekiem <- data.frame()
  x5s3_uzDivniekiem <- data.frame()
  x5s3_uzCetriniekiem <- data.frame()
  
if((x5s3$end[1] == "2" && x5s3$start[2] == "1" && x5s3$end[3] == "2" && x5s3$start[4] == "1" && x5s3$start[5] == "1") && (x5s3$NDZ_sanemsanas_datums[1] != x5s3$NDZ_sanemsanas_datums[2] && x5s3$NDZ_sanemsanas_datums[2] != x5s3$NDZ_sanemsanas_datums[3] && x5s3$NDZ_sanemsanas_datums[3] != x5s3$NDZ_sanemsanas_datums[4] && x5s3$NDZ_sanemsanas_datums[4] != x5s3$NDZ_sanemsanas_datums[5])) {
  x5s3_uzVieniniekiem <- rbind(x5s3_uzVieniniekiem, x5s3[c(1, 5), ])
  x5s3_uzDivniekiem <- rbind(x5s3_uzDivniekiem, x5s3[2:3, ])
} else if ((x5s3$end[1] == "2" && x5s3$start[2] == "1" && x5s3$start[3] == "1" && x5s3$end[4] == "2" && x5s3$start[5] == "1") && (x5s3$NDZ_sanemsanas_datums[1] != x5s3$NDZ_sanemsanas_datums[2] && x5s3$NDZ_sanemsanas_datums[2] != x5s3$NDZ_sanemsanas_datums[3] && x5s3$NDZ_sanemsanas_datums[3] != x5s3$NDZ_sanemsanas_datums[4] && x5s3$NDZ_sanemsanas_datums[4] != x5s3$NDZ_sanemsanas_datums[5])) {
  x5s3_uzVieniniekiem <- rbind(x5s3_uzVieniniekiem, x5s3[c(1, 5), ])
  x5s3_uzDivniekiem <- rbind(x5s3_uzDivniekiem, x5s3[3:4, ])
} else if (x5s3$end[1] == "2" && x5s3$start[2] == "1" && x5s3$start[3] == "1" && x5s3$start[4] == "1" && x5s3$end[5] == "2" && any(diff(x5s3$NDZ_sanemsanas_datums) != 0)) {
  x5s3_uzVieniniekiem <- rbind(x5s3_uzVieniniekiem, x5s3[1, ])
  x5s3_uzDivniekiem <- rbind(x5s3_uzDivniekiem, x5s3[4:5, ])
} else if (x5s3$start[1] == "1" && x5s3$end[2] == "2" && x5s3$start[3] == "1" && x5s3$end[4] == "2" && x5s3$start[5] == "1" && any(diff(x5s3$NDZ_sanemsanas_datums[1:3]) != 0) && x5s3$NDZ_sanemsanas_datums[4] == x5s3$NDZ_sanemsanas_datums[5]) {
  x5s3_uzVieniniekiem <- rbind(x5s3_uzVieniniekiem, x5s3[5, ])
  x5s3_uzDivniekiem <- rbind(x5s3_uzDivniekiem, x5s3[1:4, ])
} else if (x5s3$end[1] == "2" && x5s3$start[2] == "1" && x5s3$start[3] == "1" && x5s3$end[4] == "2" && x5s3$start[5] == "1" && x5s3$NDZ_sanemsanas_datums[1] == x5s3$NDZ_sanemsanas_datums[2]) {
  x5s3_uzVieniniekiem <- rbind(x5s3_uzVieniniekiem, x5s3[5, ])
  x5s3_uzDivniekiem <- rbind(x5s3_uzDivniekiem, x5s3[1:4, ])
} else if (all(x5s3$start[c(1,3,5)] == "1") && 
           all(diff(x5s3$NDZ_sanemsanas_datums[2:3]) == 0) && 
           all(diff(x5s3$NDZ_sanemsanas_datums[3:5]) != 0) && 
           all(diff(x5s3$NDZ_sanemsanas_datums[1:2]) != 0) && 
           x5s3$PS_code[1] == "__________" && x5s3$NM_code[1] == "_____________") {
  # Šis ir tas unokālais gadījums, kluram man vajadzēja visu gadu, lai izlobītu, kas tur notiek, j
  #jo indivīds ik pārdienas ņem bezalgas atvaļinājumus tā, ka atvaļinājuma sākuma dienā atsāk darbu, bet citreiz nē.
  #Nebiju pārliecināta, ka šo var vispārināt.
  x5s3_uzVieniniekiem <- rbind(x5s3_uzVieniniekiem, x5s3[5, ])
  x5s3_uzCetriniekiem <- rbind(x5s3_uzCetriniekiem, x5s3[-5, ])
} else if (all(x5s3$start[1:3] == "1") && 
           all(diff(x5s3$NDZ_sanemsanas_datums[2:3]) == 0) && 
           all(diff(x5s3$NDZ_sanemsanas_datums[1:2]) != 0) && 
           all(diff(x5s3$NDZ_sanemsanas_datums[3:5]) != 0)) {
  x5s3_uzDivniekiem <- rbind(x5s3_uzDivniekiem, x5s3[c(3,5), ])
} else if (all(x5s3$start[3:5] == "1") && 
           all(diff(x5s3$NDZ_sanemsanas_datums[1:3]) == 0) && 
           diff(x5s3$NDZ_sanemsanas_datums[4:5]) == 0 && diff(x5s3$NDZ_sanemsanas_datums[3:4]) != 0) {
  x5s3_uzVieniniekiem <- rbind(x5s3_uzVieniniekiem, x5s3[c(3,5), ])
} else if (all(x5s3$start[3:5] == "1") && 
           all(diff(x5s3$NDZ_sanemsanas_datums[1:3]) == 0) && 
           all(diff(x5s3$NDZ_sanemsanas_datums[3:5]) != 0)) {
  x5s3_uzVieniniekiem <- rbind(x5s3_uzVieniniekiem, x5s3[c(1,5), ])
} else if (all(x5s3$start[c(1,4,5)] == "1") && 
           all(sapply(seq(2,5, by=2), function(i) all(diff(x5s3$NDZ_sanemsanas_datums[i:(i+1)]) == 0))) && 
           all(sapply(c(1, 3), function(i) all(diff(x5s3$NDZ_sanemsanas_datums[i:(i+1)]) != 0)))) {
  x5s3_uzVieniniekiem <- rbind(x5s3_uzVieniniekiem, x5s3[5, ])
  x5s3_uzDivniekiem <- rbind(x5s3_uzDivniekiem, x5s3[c(1, 3), ])
} else if (all(x5s3$start[c(1,3,4)] == "1") && all(diff(x5s3$NDZ_sanemsanas_datums) != 0)) {
  x5s3_uzCetriniekiem <- rbind(x5s3_uzCetriniekiem, x5s3[c(1, 2, 4, 5), ])
} else if (all(x5s3$start[3:5] == "1") && all(diff(x5s3$NDZ_sanemsanas_datums[2:4]) != 0) &&
           all(sapply(c(1,4), function(i) diff(x5s3$NDZ_sanemsanas_datums[i:(i+1)]) == 0))) {
  x5s3_uzVieniniekiem <- rbind(x5s3_uzVieniniekiem, x5s3[c(1, 5), ])
} else {stop("ERROR: Piecinieku gadījumam iztrūkst apstrādes kods: ", paste0("rinda: ", r, "."), "\n")}


rm(x5s3)
return(list(x5s3_uzVieniniekiem = x5s3_uzVieniniekiem, 
            x5s3_uzDivniekiem = x5s3_uzDivniekiem, 
            x5s3_uzCetriniekiem = x5s3_uzCetriniekiem))
}

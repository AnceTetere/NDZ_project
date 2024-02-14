# Ja vienam un tajam pašam vienumam, vienā un tajā pašā uzņēmumā 
# ir divas atzīmes par darba sākšanu vai beigšanu, 
# tad, ja ziņojuma datumi neatšķiras, saglabā tikai vienu no tiem.

# Ja ziņojuma datumi atšķiras, atstāj vēlāko ierakstu datumu.

F_doubleStartEnd_codesMatch <- function(x2) {
rownames(x2) <- NULL
n1 <- data.frame()
n2 <- data.frame()

for (k in seq(1, nrow(x2), by = 2)) {
  if ((doublesTest(k, x2)) && (x2$zinkod[k] == x2$zinkod[k + 1])) {
    if ((x2$NDZ_sanemsanas_datums[k] == x2$NDZ_sanemsanas_datums[k + 1]) ||
        (x2$NDZ_sanemsanas_datums[k] > x2$NDZ_sanemsanas_datums[k + 1])) {
      n1 <- rbind(n1, x2[k,])
    } else {
      n2 <- rbind(n2, x2[k + 1,])
    }
  }
} 

x2_finish <- rbind(n1, n2)  

if(nrow(x2_finish) * 2 == nrow(x2)) {
  return(x2_finish)
  rm(k, n1, n2)
} else {
  stop(return(cat("ERROR: Atvasināto tabulu rindas neatbilst mātes tabulai x2!")))
}
}

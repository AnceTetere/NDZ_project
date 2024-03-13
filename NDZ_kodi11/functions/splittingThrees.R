
splittingThrees <- function(x3) {
  
  x_vieninieki <- data.frame()
  x_divnieki <- data.frame()
  
  if ((x3$zinkod[1] == "40" && x3$zinkod[2] == "41" && x3$zinkod[3] == "41") && (x3$NDZ_sanemsanas_datums[2] <= x3$NDZ_sanemsanas_datums[3]) && (x3$NDZ_sanemsanas_datums[1] != x3$NDZ_sanemsanas_datums[2])) {
    x_vieninieki <- rbind(x_vieninieki, x3[c(1, 3), ]) # Nenobrīnies par šo, no kodiem 40, mēs kā vieniniekus šos rēķinām un galā uz personu tie sasummējas.
  } else if ((x3$start[1] == "1" && x3$end[2] == "2" && x3$sak_darbu[1] <= x3$beidz_darbu[2]) || (x3$end[1] == "2" && x3$start[2] == "1" && x3$NDZ_sanemsanas_datums[1] == x3$NDZ_sanemsanas_datums[2]) && x3$end[3] != "2") {
    x_divnieki <- rbind(x_divnieki, x3[1:2, ])
    x_vieninieki <- rbind(x_vieninieki, x3[3, ])
  } else if (((x3$start[2] == "1" && x3$end[3] == "2") && (x3$sak_darbu[2] <= x3$beidz_darbu[3])) || (x3$end[1] == "2" && (((x3$start[2] == "1" && x3$end[3] == "2") && (x3$NDZ_sanemsanas_datums[2] <= x3$NDZ_sanemsanas_datums[3])) || ((x3$end[2] == "2" && x3$start[3] == "1") && (x3$NDZ_sanemsanas_datums[2] == x3$NDZ_sanemsanas_datums[3]))))) {
    x_divnieki <- rbind(x_divnieki, x3[c(2, 3), ])
    x_vieninieki <- rbind(x_vieninieki, x3[c(1), ])
  } else if ((x3$start[1] == "1" && x3$end[2] == "2" && x3$end[3] == "2") && ((x3$sak_darbu[1] <= x3$beidz_darbu[2]) && (x3$beidz_darbu[2] <= x3$beidz_darbu[3]))) {
    x_divnieki <- rbind(x_divnieki, x3[c(1, 3), ])
  } else if ((x3$end[1] == "2" && x3$end[2] == "2" && x3$start[3] == "1") && ((x3$beidz_darbu[1] <= x3$beidz_darbu[2]) && (x3$beidz_darbu[2] <= x3$sak_darbu[3]))) {
    x_divnieki <- rbind(x_divnieki, x3[c(2, 3), ])
  } else if ((x3$end[1] == "2" && x3$start[2] == "1" && x3$start[3] == "1") && (x3$beidz_darbu[1] != x3$sak_darbu[3])) {
    x_vieninieki <- rbind(x_vieninieki, x3[c(1, 3), ])
  } else {
    stop(cat("No trijniekiem sūtītajā tabulā uz splittingThrees trūkst apstrādes koda!"))
  }

  return(list(x_vieninieki = x_vieninieki, x_divnieki = x_divnieki))
}

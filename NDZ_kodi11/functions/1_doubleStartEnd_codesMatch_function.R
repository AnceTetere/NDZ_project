F_doubleStartEnd_codesMatch <- function(x2) {
rownames(x2) <- NULL

n <- data.frame()

for (k in seq(1, nrow(x2), by = 2)) {
  if (doublesTest(k, x2) && x2$zinkod[k] == x2$zinkod[k + 1]) { 
    if (diff(x2$NDZ_sanemsanas_datums[k:(k+1)]) <= 0) {
      n <- rbind(n, x2[k,])
    } else {
      n <- rbind(n, x2[k + 1,])
    }
  } else {
    stop("Pārbaude nav izieta: Funkcija F_doubleStartEnd_codesMatch(). \n")
  }
} 

if(nrow(n) * 2 != nrow(x2)) {
  stop("ERROR: Funkcijā F_doubleStartEnd_codesMatch() atvasināto tabulu rindas neatbilst mātes tabulai x2! \n")
  } else {
    rm(k, x2)
    return(n)
  }
}

starpkodi2 <- function(y, t, prev, v) {
#for tests y <- y_2plus
  
  #if (diff(t$NDZ_sanemsanas_datums[1:2]) == 0) {
  #  yt <- starpkodi2_vienadiDatumi(y2, t, prev, v)
  #} else
  if (t$zinkod[1] == "11") {
    yt <- starpkodi2_11(y, t, prev, v)
  } else if (t$zinkod[1] == "21") {
    yt <- starpkodi2_21(y, t, prev, v)
  } else if (t$zinkod[1] == "25") {
    yt <- starpkodi2_25(y, t, prev, v)
  } else if (t$zinkod[1] == "40") {
    yt <- starpkodi2_40(y, t, prev, v)
  } else if (t$zinkod[1] == "41") {
    yt <- starpkodi2_41(y, t, prev, v)
  } else if (t$zinkod[1] == "51") {
      yt <- starpkodi2_51(y, t, prev, v)
  #} else if (t$zinkod[1] == "53") {
  #  yt <- starpkodi2_53(y2, t, prev, v)
  } else if (t$zinkod[1] == "50") {
    yt <- starpkodi2_50(y, t, prev, v)
  } else if (t$zinkod[1] == "92") {
    yt <- starpkodi2_92(y, t, prev, v)
  #} else if (t$zinkod[1] == "54" && t$zinkod[2] == "21") {
  #  yt <- y2[v,]
  #  yt$dienas <- as.numeric(difftime(t$beidz_darbu[2], t$sak_darbu[1], units = "days")) + 1
  #} else if (t$zinkod[1] == "54" && t$zinkod[2] == "25") {
  #  yt <- y2[v,] 
  #  yt$dienas <- as.numeric(difftime(t$beidz_darbu[2], t$sak_darbu[1], units = "days")) + 1 
    #} else if (t$zinkod[1] == "91" && t$zinkod[2] == "21") {
  #  yt <- y2[v,] 
  #  yt$dienas <- as.numeric(difftime(t$beidz_darbu[1], prev, units = "days")) - 1 
    
  } else {stop("Starpkodi2: Trūkst izstrādes koda.")}
  
  if(is.na(yt$PS_code[1])) {stop("Dienas NA.")}
  yt$zinkod <- "combined"  #jo starpkodu dienu sarēķins
  return(yt)
}

starpkodi2 <- function(y, t, prev, v) {
  
  if (diff(t$NDZ_sanemsanas_datums[1:2]) == 0) {
     yt <- starpkodi2_vienadiDatumi(y, t, prev, v)
  } else if (t$zk[1] == "11") {
    yt <- starpkodi2_11(y, t, prev, v)
  } else if (t$zk[1] %in% c("21", "22", "23", "24", "25", "29")) {
    yt <- starpkodi2_25(y, t, prev, v)
  } else if (t$zk[1] == "40") {
    yt <- starpkodi2_40(y, t, prev, v)
  } else if (t$zk[1] %in% c("41", "51", "54", "92")) {
      yt <- starpkodi2_51(y, t, prev, v)
  #} else if (t$zk[1] == "53") {
  #  yt <- starpkodi2_53(y2, t, prev, v)
  } else if (t$zk[1] == "50") {
    yt <- starpkodi2_50(y, t, prev, v)
  #} else if (t$zk[1] %in% c("41", "51", "54", "92")) && t$zk[2] == "21") {
  #  yt <- y2[v,]
  #  yt$dd <- as.numeric(difftime(t$beidz[2], t$sak[1], units = "days")) + 1
  #} else if (t$zk[1] == "54" && t$zk[2] == "25") {
  #  yt <- y2[v,] 
  #  yt$dd <- as.numeric(difftime(t$beidz[2], t$sak[1], units = "days")) + 1 
    #} else if (t$zk[1] == "91" && t$zk[2] == "21") {
  #  yt <- y2[v,] 
  #  yt$dd <- as.numeric(difftime(t$beidz[1], prev, units = "days")) - 1 
    
  } else {stop("Starpkodi2: Trūkst izstrādes koda.")}
  
  if(is.na(yt$PS_code[1])) {stop("Dienas NA.")}
  yt$zk <- "combined"  #jo starpkodu dienu sarēķins
  return(yt)
}

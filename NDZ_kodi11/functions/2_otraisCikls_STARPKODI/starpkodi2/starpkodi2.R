starpkodi2 <- function(y, t, prev, v) {
  
  if (diff(t$NDZ_sanemsanas_datums[1:2]) == 0) {
     yt <- starpkodi2_vienadiDatumi(y, t, prev, v)
  } else if (t$zk[1] %in% c("11", "14", "16", "61")) {
    yt <- starpkodi2_11(y, t, prev, v)
  } else if (t$zk[1] %in% c("21", "22", "23", "24", "25", "29")) {
    yt <- starpkodi2_25(y, t, prev, v)
  } else if (t$zk[1] %in% c("40", "50", "53", "91")) {
    yt <- starpkodi2_40(y, t, prev, v)
  } else if (t$zk[1] %in% c("41", "51", "54", "92")) {
      yt <- starpkodi2_51(y, t, prev, v)
  } else {stop("Starpkodi2: Trūkst izstrādes koda.")}
  
  if(is.na(yt$PS_code[1])) {stop("DD NA.")}
  yt$zk <- "combined"  
  return(yt)
}

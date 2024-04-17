starpkodi2_51 <- function(y2, t, prev, v) {
  if (t$zinkod[2] == "25" && t$NDZ_sanemsanas_datums[1] <= t$NDZ_sanemsanas_datums[2]) {
    yt <- y2[v,]
    yt$dienas <- as.numeric(difftime(t$beidz[2], t$sak[1], units = "days")) + 1
  } else if (t$zinkod[2] == "21") {
    yt <- y2[v,]
    yt$dienas <- as.numeric(difftime(as.Date(t$beidz[2]), as.Date(t$sak)[1], units = "days"))
  } else if (t$zinkod[2] == "23" && t$NDZ_sanemsanas_datums[1] != t$NDZ_sanemsanas_datums[2]) {
    yt <- y2[v,]
    yt$dienas <- as.numeric(difftime(as.Date(t$beidz)[2], as.Date(t$sak)[1], units = "days"))
  } else if (t$zinkod[2] == "22" && t$NDZ_sanemsanas_datums[1] <= t$NDZ_sanemsanas_datums[2]) {
    yt <- y2[v,] 
    yt$dienas <- as.numeric(difftime(as.Date(t$beidz[2]), as.Date(t$sak[1]), units = "days")) + 1
  } else if (t$zinkod[2] == "91" && t$NDZ_sanemsanas_datums[1] != t$NDZ_sanemsanas_datums[2]) {
    yt <- y2[v,] 
    yt$dienas <- as.numeric(difftime(as.Date(t$beidz[2]), as.Date(t$sak[1]), units = "days"))
  } else {
    stop("starpkodi2_51: Trūkst izstrādes koda starpkodi2.")
  }
 
  if(is.na(yt$PS_code[1])) {stop("Dienas NA.")}
  return(yt)
}

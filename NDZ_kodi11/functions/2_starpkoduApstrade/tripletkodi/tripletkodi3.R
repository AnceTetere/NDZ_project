tripletkodi3 <- function(y3, t, prev, v) {
  
  if (t$zinkod[1] == "51" && t$zinkod[2] == "53" && t$zinkod[3] == "21" && 
      t$NDZ_sanemsanas_datums[1] == t$NDZ_sanemsanas_datums[2] &&
      t$NDZ_sanemsanas_datums[2] != t$NDZ_sanemsanas_datums[3]) {
    yt <- y3[v, ]
    yt$dienas <- 1
  } else if (t$zinkod[1] == "25" && t$zinkod[2] == "51" && t$zinkod[3] == "53" && 
             all(diff(t$NDZ_sanemsanas_datums) == 0)) {
    yt <- y3[v, ]
    yt$dienas <- 0
  } else if (t$zinkod[1] == "25" && t$zinkod[2] == "41" && t$zinkod[3] == "50" && 
             all(diff(t$NDZ_sanemsanas_datums) == 0)) {
    yt <- y3[v, ]
    yt$dienas <- 0
  } else if (t$zinkod[1] == "41" && t$zinkod[2] == "50" && t$zinkod[3] == "21" && 
             all(diff(t$NDZ_sanemsanas_datums) != 0)) {
    yt <- y3[v, ]
    yt$dienas <- as.numeric(difftime(t$beidz[2], t$sak[1], units = "days"))
  } else if (t$zinkod[1] == "11" && t$zinkod[2] == "50" && t$zinkod[3] == "53" && all(diff(t$NDZ_sanemsanas_datums) == 0)) {
    yt <- y3[v, ]
    yt$dienas <- 0
  } else if (t$zinkod[1] == "40" && t$zinkod[2] == "50" && t$zinkod[3] == "21" && 
             diff(t$NDZ_sanemsanas_datums[2:3]) != 0 && diff(t$NDZ_sanemsanas_datums[1:2]) == 0) {
    yt <- y3[v, ]
    yt$dienas <- as.numeric(difftime(t$beidz[1], prev, units = "days")) - 1 
  } else if (t$zinkod[1] == "11" && t$zinkod[2] == "41" && t$zinkod[3] == "50" && 
             diff(t$NDZ_sanemsanas_datums[2:3]) == 0 && diff(t$NDZ_sanemsanas_datums[1:2]) != 0 &&
             t$PS_code[1] == '___________' && t$NM_code[1] == '__________') {
    yt <- y3[v, ]
    yt$dienas <- as.numeric(difftime(t$beidz[3], t$sak[1], units = "days"))
  } else if (t$zinkod[1] == "25" && t$zinkod[2] == "41" && t$zinkod[3] == "51" && 
             all(diff(t$NDZ_sanemsanas_datums) == 0)) {
    yt <- y3[v, ]
    yt$dienas <- 0
  } else if (t$zinkod[1] == "11" && t$zinkod[2] == "51" && t$zinkod[3] == "40" && 
             all(diff(t$NDZ_sanemsanas_datums) != 0) && t$PS_code[1] == '_____________' && t$NM_code[1] == '___________') {
    yt <- y3[v, ]
    yt$dienas <- as.numeric(difftime(t$beidz[3], t$sak[1], units = "days"))
  } else if (t$zinkod[1] == "51" && t$zinkod[2] == "53" && t$zinkod[3] == "25" && 
             all(diff(t$NDZ_sanemsanas_datums) != 0)) {
    yt <- y3[v, ]
    yt$dienas <- as.numeric(difftime(t$beidz[2], t$sak[1], units = "days"))
  } else if (t$zinkod[1] == "21" && t$zinkod[2] == "41" && t$zinkod[3] == "53" && 
             all(diff(t$NDZ_sanemsanas_datums) == 0)) {
    yt <- y3[v, ]
    yt$dienas <- 0
  } else if (t$zinkod[1] == "11" && t$zinkod[2] == "40" && t$zinkod[3] == "50" && 
             all(diff(t$NDZ_sanemsanas_datums) == 0)) {
    yt <- y3[v, ]
    yt$dienas <- 0
  } else {
    stop("Tripletkodi3 iztrūkst apstrādes koda.")
  }
  
  if(is.na(yt$PS_code[1])) {stop("Dienas NA.")}
  yt$zinkod <- "combined"  #jo tripletkodu dienu sarēķins
  return(yt)
}

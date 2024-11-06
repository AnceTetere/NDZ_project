starpkodi3_11_51 <- function(y2, t, prev, v) {
  
  if (t$zinkod[2] == "51" && t$zinkod[3] == "51" && diff(t$NDZ_sanemsanas_datums[2:3]) != 0 && diff(t$NDZ_sanemsanas_datums[1:2]) == 0 && 
             t$PS_code[1] == '____________' && t$NM_code[1] == '____________') {
    yt <- y2[v, ]
    yt$dienas <- as.numeric(difftime(t$last_date[3], t$sak_darbu[3], units = "days")) + 1
  } else if (t$zinkod[2] == "41" && t$zinkod[3] == "40" && diff(t$NDZ_sanemsanas_datums[1:2]) == 0 && diff(t$NDZ_sanemsanas_datums[2:3]) != 0 &&
             t$PS_code[1] == '____________' && t$NM_code[1] == '____________') {
    yt <- y2[v, ]
    yt$dienas <- as.numeric(difftime(t$beidz_darbu[3], t$sak_darbu[1], units = "days"))
  } else if (t$zinkod[2] == "51" && t$zinkod[3] == "51" && t$PS_code[1] == '____________' && t$NM_code[1] == '____________') {
    yt <- y2[v, ]
    yt$dienas <- as.numeric(difftime(t$last_date[3], t$sak_darbu[3], units = "days")) + 1
  } else if (t$zinkod[2] == "51" && t$zinkod[3] == "50" && diff(t$NDZ_sanemsanas_datums[2:3]) == 0) {
    yt <- y2[v, ]
    yt$dienas <- as.numeric(difftime(t$last_date[3], t$sak_darbu[2], units = "days")) + 1
  } else if (t$zinkod[2] == "41" && t$zinkod[3] == "40" && all(diff(t$NDZ_sanemsanas_datums) == 0)) {
    yt <- y2[v, ]
    yt$dienas <- as.numeric(difftime(t$last_date[3], t$sak_darbu[1], units = "days")) + 1
  } else {
    stop("Starpkodi3_11_51: Trūkst izstrādes koda.")
  }
  
  if(is.na(yt$PS_code[1])) {stop("Dienas NA.")}
  return(yt) 
}

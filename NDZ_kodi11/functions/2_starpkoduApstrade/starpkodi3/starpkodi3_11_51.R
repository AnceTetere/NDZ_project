starpkodi3_11_51 <- function(y2, t, prev, v) {
  
  if (t$zk[3] %in% c("40", "50", "53", "91")) { 
    if (diff(t$NDZ_sanemsanas_datums[1:2]) != 0 && diff(t$NDZ_sanemsanas_datums[2:3]) == 0) {
    yt <- y2[v, ]
    yt$dd <- as.numeric(sum(diff(t$NDZ_sanemsanas_datums[c(1,3)]), 
                                difftime(t$last_date[3], t$NDZ_sanemsanas_datums[2], units = "days") + 1))
  } else if (all(diff(t$NDZ_sanemsanas_datums) == 0)) {
    yt <- y2[v, ]
    yt$dd <- as.numeric(difftime(t$last_date[3], t$NDZ_sanemsanas_datums[1], units = "days")) + 1
  } else {stop("Starpkodi3_11_51: Trūkst izstrādes koda.")}
  } else {stop("Starpkodi3_11_51: Trūkst izstrādes koda.")}
  
  if(is.na(yt$PS_code[1])) {stop("DD NA.")}
  return(yt) 
}

#if (t$zk[3] == "51" && diff(t$NDZ_sanemsanas_datums[2:3]) != 0 && diff(t$NDZ_sanemsanas_datums[1:2]) == 0 && 
#           t$PS_code[1] == '___________' && t$nmrkod[1] == '___________') {
#  yt <- y2[v, ]
#  yt$dd <- as.numeric(difftime(t$last_date[3], t$NDZ_sanemsanas_datums[3], units = "days")) + 1
#} else if (t$zk[3] == "40" && diff(t$NDZ_sanemsanas_datums[1:2]) == 0 && diff(t$NDZ_sanemsanas_datums[2:3]) != 0 &&
#           t$PS_code[1] == '___________' && t$nmrkod[1] == '___________') {
#  yt <- y2[v, ]
#  yt$dd <- as.numeric(difftime(t$NDZ_sanemsanas_datums[3], t$NDZ_sanemsanas_datums[1], units = "days"))
#} else if (t$zk[3] == "51" && t$PS_code[1] == '___________' && t$nmrkod[1] == '___________') {
#  yt <- y2[v, ]
#  yt$dd <- as.numeric(difftime(t$last_date[3], t$NDZ_sanemsanas_datums[3], units = "days")) + 1
#} else 

starpkodi2_51 <- function(y, t, prev, v) {

  yt <- y[v,]

  if (t$zk[2] %in% c("11", "14", "16", "61")) {
           if (t$PS_code[1] == '__________' && t$NM_code[1] == '__________') {
             yt$dd <- 0
           } else if (t$PS_code[1] == '__________' && t$NM_code[1] == '__________') {
             yt <- y[v,] 
             yt$dd <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")),
                              as.numeric(difftime(t$last_date[2], t$NDZ_sanemsanas_datums[2], units = "days")))
           } else if (t$PS_code[1] == '__________' && t$NM_code[1] == '__________' && t$period[1] == "202101") {
             yt <- y[v:(v+1),]
             yt <- yt[yt$zk == "11",]
           } else {stop("starpkodi2_51: Trūkst izstrādes koda.")}
  } else if (t$zk[2] %in% c("21", "22", "23", "24", "25", "29")) {
            yt$dd <- as.numeric(diff(t$NDZ_sanemsanas_datums[1:2]) + 1)
  } else if (t$zk[2] %in% c("40", "50", "53", "91")) {
            yt$dd <- as.numeric(diff(t$NDZ_sanemsanas_datums[1:2]))
  } else if (t$zk[2] %in% c("41", "51", "54", "92")) {
    if (t$PS_code[1] == '__________' && t$NM_code[1] == '__________') {
      yt$dd <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")),
                       as.numeric(difftime(t$last_date[2], t$NDZ_sanemsanas_datums[2], units = "days"))) #tie divi t$NDZ_sanemsanas_datums abos aprēķinos NAV kļūda
    } else if (t$period[1] == "202201" && t$PS_code[1] == '__________' && t$NM_code[1] == '__________') {
      yt$dd <- as.numeric(difftime(t$last_date[2], t$NDZ_sanemsanas_datums[2], units = "days")) + 1
    } else {stop("starpkodi2_51: Trūkst izstrādes koda.")}
  } else {stop("starpkodi2_51: Trūkst izstrādes koda.")}
    

  rm(y, t, prev, v)
  return(yt)
}

#} else if (t$zk[2] == "21") {
#  yt <- y2[v,]
#  yt$dd <- as.numeric(difftime(t$beidz[2], t$sak[1], units = "days")) + 1
#} else if (t$zk[1] == "54" && t$zk[2] == "25") {
#  yt <- y2[v,] 
#  yt$dd <- as.numeric(difftime(t$beidz[2], t$sak[1], units = "days")) + 1

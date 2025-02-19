starpkodi2_51 <- function(y, t, prev, v) {

  yt <- y[v,]

  if (t$zinkod[2] %in% c("11", "14", "16", "61")) {
           if (t$PS_code[1] == '____________' && t$NM_code[1] == '____________') {
             yt$dd <- 0
           } else if ((t$PS_code[1] == '____________' && t$NM_code[1] == '____________') ||
                      (t$PS_code[1] == '____________' && t$NM_code[1] == '____________')) {
             yt$dd <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "dd")),
                              as.numeric(difftime(t$last_date[2], t$NDZ_sanemsanas_datums[2], units = "dd"))) + 1 
           } else if (t$PS_code[1] == '____________' && t$NM_code[1] =____________' && t$period[1] == "") {
             t$zinkod[t$zinkod=="11"] <- "50"
             t$sak_beidz[t$zinkod=="50"] <- "2"
             yt$dd <- as.numeric(diff(t$NDZ_sanemsanas_datums))
             ZERO_plus(t %>% slice(2))
             ZERO_minus(t %>% slice(1))
           } else if (t$PS_code[1] == '____________' && t$NM_code[1] == '____________' && t$period[1] == "______") {
             yt <- y[v:(v+1),]
             yt <- yt[yt$zinkod == "11",]
           } else if (t$PS_code[1] == '____________' && t$NM_code[1] == '____________' && t$period[1] == "______") {
             yt$dd <- as.numeric(difftime(t$last_date[2], t$NDZ_sanemsanas_datums[2], units = "dd")) + 1
           } else if (t$PS_code[1] == '____________' && t$NM_code[1] == '____________' && t$period[1] == "______") {
             yt$dd <- as.numeric(difftime(t$last_date[2], t$NDZ_sanemsanas_datums[2], units = "dd")) + 1
             ZERO_minus(t %>% slice(1))
           } else {stop("starpkodi2_51: Trūkst izstrādes koda.")}
  } else if (t$zinkod[2] %in% c("21", "22", "23", "24", "25", "29")) {
            yt$dd <- as.numeric(diff(t$NDZ_sanemsanas_datums[1:2]) + 1)
  } else if (t$zinkod[2] %in% c("40", "50", "53", "91")) {
            yt$dd <- as.numeric(diff(t$NDZ_sanemsanas_datums[1:2]))
  } else if (t$zinkod[2] %in% c("41", "51", "54", "92")) {
             if (t$PS_code[1] == '____________' && t$NM_code[1] == '____________') {
               yt$dd <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "dd")),
                                as.numeric(difftime(t$last_date[2], t$NDZ_sanemsanas_datums[2], units = "dd")))
             } else if (t$period[1] == "______" && t$PS_code[1] == '____________' && t$NM_code[1] == '____________') {
               yt$dd <- as.numeric(difftime(t$last_date[2], t$NDZ_sanemsanas_datums[1], units = "dd")) + 1
             } else if (t$period[1] == "______" && t$PS_code[1] == '____________' && t$NM_code[1] == '____________') {
               yt$dd <- as.numeric(difftime(t$last_date[2], t$NDZ_sanemsanas_datums[2], units = "dd")) + 1
             } else {stop("starpkodi2_51: Trūkst izstrādes koda.")}
  } else {stop("starpkodi2_51: Trūkst izstrādes koda.")}
    

  rm(y, t, prev, v)
  return(yt)
}

#} else if (t$zinkod[2] == "21") {
#  yt <- y2[v,]
#  yt$dd <- as.numeric(difftime(t$beidz_darbu[2], t$sak_darbu[1], units = "dd")) + 1
#} else if (t$zinkod[1] == "54" && t$zinkod[2] == "25") {
#  yt <- y2[v,] 
#  yt$dd <- as.numeric(difftime(t$beidz_darbu[2], t$sak_darbu[1], units = "dd")) + 1 

starpkodi2_51 <- function(y, t, prev, v) {

  yt <- y[v,]

  if (t$zinkod[2] %in% c("11", "14", "16", "61")) {
           if (t$PS_code[1] == '_______________' && t$NM_code[1] == '_______________') {
             yt$dienas <- 0
           } else if (t$PS_code[1] == '_______________' && t$NM_code[1] == '_______________') {
             yt <- y[v,] 
             yt$dienas <- sum(as.numeric(difftime(t$ZDN_sanemsanas_datums[1], prev, units = "days")),
                              as.numeric(difftime(t$last_date[2], t$ZDN_sanemsanas_datums[2], units = "days")))
           } else if (t$PS_code[1] == '_______________' && t$NM_code[1] == '_______________' && t$period[1] == "_______________") {
             t$zinkod[t$zinkod=="11"] <- "50"
             t$sak_beidz[t$zinkod=="50"] <- "2"
             yt$dienas <- as.numeric(diff(t$ZDN_sanemsanas_datums))
             ZERO_plus(t %>% slice(2))
             ZERO_minus(t %>% slice(1))
           } else if (t$PS_code[1] == '_______________' && t$NM_code[1] == '_______________' && t$period[1] == "_______________") {
             yt <- y[v:(v+1),]
             yt <- yt[yt$zinkod == "11",]
           } else {stop("starpkodi2_51: Trūkst izstrādes koda.")}
  } else if (t$zinkod[2] %in% c("21", "22", "23", "24", "25", "29")) {
            yt$dienas <- as.numeric(diff(t$ZDN_sanemsanas_datums[1:2]) + 1)
  } else if (t$zinkod[2] %in% c("40", "50", "53", "91")) {
            yt$dienas <- as.numeric(diff(t$ZDN_sanemsanas_datums[1:2]))
  } else if (t$zinkod[2] %in% c("41", "51", "54", "92")) {
             if (t$PS_code[1] == '_______________' && t$NM_code[1] == '_______________') {
               yt$dienas <- sum(as.numeric(difftime(t$ZDN_sanemsanas_datums[1], prev, units = "days")),
                                as.numeric(difftime(t$last_date[2], t$ZDN_sanemsanas_datums[2], units = "days"))) 
             } else if (t$period[1] == "_______________" && t$PS_code[1] == '_______________' && t$NM_code[1] == '_______________') {
               yt$dienas <- as.numeric(difftime(t$last_date[2], t$ZDN_sanemsanas_datums[2], units = "days")) + 1
             } else {stop("starpkodi2_51: Trūkst izstrādes koda.")}
  } else {stop("starpkodi2_51: Trūkst izstrādes koda.")}
    

  rm(y, t, prev, v)
  return(yt)
}

#} else if (t$zinkod[2] == "21") {
#  yt <- y2[v,]
#  yt$dienas <- as.numeric(difftime(t$beidz_darbu[2], t$sak_darbu[1], units = "days")) + 1
#} else if (t$zinkod[1] == "54" && t$zinkod[2] == "25") {
#  yt <- y2[v,] 
#  yt$dienas <- as.numeric(difftime(t$beidz_darbu[2], t$sak_darbu[1], units = "days")) + 1

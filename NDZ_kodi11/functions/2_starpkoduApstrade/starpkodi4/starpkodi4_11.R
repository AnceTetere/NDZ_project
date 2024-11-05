starpkodi4_11 <- function(y, t, prev, v) {
  
  if (t$zinkod[2] %in% c("40", "50", "53", "91")) {
    yt <- starpkodi4_11_50(y, t, prev, v)
  } else if (t$zinkod[2] %in% c("41", "51", "54", "92")) {
    yt <- starpkodi4_11_91(y, t, prev, v)
  } else if (t$zinkod[2] %in% c("21", "22","23", "24", "25", "29")) {
    yt <- starpkodi4_11_25(y, t, prev, v)
  } else {stop("Starpkodi4_11: Trūkst izstrādes koda.")}
    
#  } else if (t$zinkod[2] == "53") {
#    yt <- starpkodi4_11_53(y, t, prev, v)
#  } else if (t$zinkod[2] == "11") {
#    yt <- starpkodi4_11_11(y, t, prev, v)
#  } else if (t$zinkod[2] == "26" && t$zinkod[3] == "11" && t$zinkod[4] == "50" && 
#             all(diff(t$NDZ_sanemsanas_datums[2:3]) != 0) && t$NDZ_sanemsanas_datums[1] == t$NDZ_sanemsanas_datums[2]) {
#    yt <- y[v, ]
#    yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[4], t$NDZ_sanemsanas_datums[3], units = "days"))
#  } else if (t$zinkod[2] == "40" && t$zinkod[3] == "41" && t$zinkod[4] == "25" && 
#             all(diff(t$NDZ_sanemsanas_datums[2:4]) != 0) && t$NDZ_sanemsanas_datums[1] == t$NDZ_sanemsanas_datums[2]) {
#    yt <- y[v, ] 
#    yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[4], t$NDZ_sanemsanas_datums[3], units = "days")) + 1 
#  } else if (t$zinkod[2] == "41" && t$zinkod[3] == "40" && t$zinkod[4] == "41" && 
#             diff(t$NDZ_sanemsanas_datums[3:4]) != 0 && all(diff(t$NDZ_sanemsanas_datums[1:3]) == 0) &&
#             t$PS_code[1] == '_________' && t$NM_code[1] == '_________') {
#    yt <- y[v, ] 
#    yt$dienas <- as.numeric(difftime(t$last_date[4], t$NDZ_sanemsanas_datums[4], units = "days")) + 1 
#   } else if (t$zinkod[2] == "92" && t$zinkod[3] == "40" && t$zinkod[4] == "41" && 
#              all(diff(t$NDZ_sanemsanas_datums) == 0) && t$PS_code[1] == '_________' && t$NM_code[1] == '_________') {
#     yt <- y[v, ]
#     yt$dienas <- 0
#   } else if (t$zinkod[2] == "51" && t$zinkod[3] == "50" && t$zinkod[4] == "25" && 
#              all(diff(t$NDZ_sanemsanas_datums[1:3]) == 0) && diff(t$NDZ_sanemsanas_datums[3:4]) != 0) {
#     yt <- y[v:(v+1), ]
#     yt <- yt[yt$zinkod == "11", ]
#   } else if (t$zinkod[2] == "40" && t$zinkod[3] == "41" && t$zinkod[4] == "40" && all(diff(t$NDZ_sanemsanas_datums) != 0)) {
#     yt <- y[v, ]
#     yt$dienas <- sum(sapply(seq(1,4,by=2), function(i) as.numeric(difftime(t$NDZ_sanemsanas_datums[i+1], t$NDZ_sanemsanas_datums[i], units = "days"))))
#   } else if (t$zinkod[2] == "40" && t$zinkod[3] == "25" && t$zinkod[4] == "41" && 
#              diff(t$NDZ_sanemsanas_datums[2:3]) != 0 && all(sapply(seq(1,4, by=2), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0))) {
#     yt <- y[v, ]
#     yt$dienas <- 0

  return(yt) 
}

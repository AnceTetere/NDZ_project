starpkodi4_11 <- function(y, t, prev, v) {
  
  if (t$zk[2] %in% c("40", "50", "53", "91")) {
    yt <- starpkodi4_11_50(y, t, prev, v)
  } else if (t$zk[2] %in% c("41", "51", "54", "92")) {
    yt <- starpkodi4_11_91(y, t, prev, v)
  } else if (t$zk[2] %in% c("21", "22","23", "24", "25", "29")) {
    yt <- starpkodi4_11_25(y, t, prev, v)
  } else if (t$zk[2] %in% c("11", "14", "16", "61")) {
    yt <- starpkodi4_11_11(y, t, prev, v)
  } else {stop("Starpkodi4_11: Trūkst izstrādes koda.")}
    
#  } else if (t$zk[2] == "53") {
#    yt <- starpkodi4_11_53(y, t, prev, v)
#  } else if (t$zk[2] == "26" && t$zk[3] == "11" && t$zk[4] == "50" && 
#             all(diff(t$NDZ_sanemsanas_datums[2:3]) != 0) && t$NDZ_sanemsanas_datums[1] == t$NDZ_sanemsanas_datums[2]) {
#    yt <- y[v, ]
#    yt$dd <- as.numeric(difftime(t$NDZ_sanemsanas_datums[4], t$NDZ_sanemsanas_datums[3], units = "days"))
#  } else if (t$zk[2] == "40" && t$zk[3] == "41" && t$zk[4] == "25" && 
#             all(diff(t$NDZ_sanemsanas_datums[2:4]) != 0) && t$NDZ_sanemsanas_datums[1] == t$NDZ_sanemsanas_datums[2]) {
#    yt <- y[v, ] 
#    yt$dd <- as.numeric(difftime(t$NDZ_sanemsanas_datums[4], t$NDZ_sanemsanas_datums[3], units = "days")) + 1 
#  } else if (t$zk[2] == "41" && t$zk[3] == "40" && t$zk[4] == "41" && 
#             diff(t$NDZ_sanemsanas_datums[3:4]) != 0 && all(diff(t$NDZ_sanemsanas_datums[1:3]) == 0) &&
#             t$PS_code[1] == '____________' && t$NM_code[1] == '____________') {
#    yt <- y[v, ] 
#    yt$dd <- as.numeric(difftime(t$last_date[4], t$NDZ_sanemsanas_datums[4], units = "days")) + 1 
#   } else if (t$zk[2] == "92" && t$zk[3] == "40" && t$zk[4] == "41" && 
#              all(diff(t$NDZ_sanemsanas_datums) == 0) && t$PS_code[1] == '____________' && t$NM_code[1] == '____________') {
#     yt <- y[v, ]
#     yt$dd <- 0
#   } else if (t$zk[2] == "51" && t$zk[3] == "50" && t$zk[4] == "25" && 
#              all(diff(t$NDZ_sanemsanas_datums[1:3]) == 0) && diff(t$NDZ_sanemsanas_datums[3:4]) != 0) {
#     yt <- y[v:(v+1), ]
#     yt <- yt[yt$zk == "11", ]
#   } else if (t$zk[2] == "40" && t$zk[3] == "41" && t$zk[4] == "40" && all(diff(t$NDZ_sanemsanas_datums) != 0)) {
#     yt <- y[v, ]
#     yt$dd <- sum(sapply(seq(1,4,by=2), function(i) as.numeric(difftime(t$NDZ_sanemsanas_datums[i+1], t$NDZ_sanemsanas_datums[i], units = "days"))))
#   } else if (t$zk[2] == "40" && t$zk[3] == "25" && t$zk[4] == "41" && 
#              diff(t$NDZ_sanemsanas_datums[2:3]) != 0 && all(sapply(seq(1,4, by=2), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0))) {
#     yt <- y[v, ]
#     yt$dd <- 0

  return(yt) 
}

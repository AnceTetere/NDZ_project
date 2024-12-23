starpkodi2_vienadiDatumi <- function(y, t, prev, v) {
if ((t$zk[1] == "11" && t$zk[2] %in% c("40", "50", "53", "91")) ||
    (t$zk[1] %in% c("21", "22", "23", "24", "25", "29") && t$zk[2]  %in% c("41", "51", "54", "92")) ||   
#    (t$zk[1] == "40" && t$zk[2] == "51") ||
    (t$zk[1] %in% c("41", "54", "51", "92") && t$zk[2] %in% c("40", "50", "53", "91"))) {#||
#    (t$zk[1] == "51" && t$zk[2] == "53") ||
#    (t$zk[1] == "92" && t$zk[2] == "50")) 
#{
  yt <- y[v,] 
  yt$dd <- 0
#} else if (t$zk[1] == "21" && t$zk[2] == "53"){
#  yt <- y[v:(v + 1),]
#  yt <- yt[yt$zk == "11",]
} else if (t$zk[1] %in% c("21", "25", "29") && t$zk[2] %in% c("40", "50", "53")) {
  yt <- y[v:(v+1),] 
  yt <- yt[yt$zk %in% c("40", "50", "53"), ] 
} else if (t$zk[1] == "11" && t$zk[2] %in% c("41", "51", "92")) {
  yt <- y[v, ]
  yt$dd <- as.numeric(difftime(t$last_date[2], t$NDZ_sanemsanas_datums[1], units = "days")) + 1
#} else if (t$zk[1] == "50" && t$zk[2] == "53" && t$PS_code[1] == '_________' && t$NM_code[1] == '_________') {
#  yt <- y[v:(v+1), ]
#  yt <- yt[yt$zk == "50", ]
#} else if (t$zk[1] == "25" && t$zk[2] == "40") {
#  yt <- y[v:(v+1), ]
#  yt <- yt[yt$zk == "40", ] 
} else if (t$zk[1] %in% c("40", "50", "53", "91") && t$zk[2] %in% c("40", "50", "53", "91")) {
  yt <- y[v, ]
  yt$dd <- as.numeric(difftime(t$NDZ_sanemsanas_datums[2], prev, units = "days")) - 1
#} else if (t$zk[1] == "51" && t$zk[2] == "54" &&
#           t$PS_code[1] == '_________' && t$NM_code[1] == '_________') {
#  yt <- y[v:(v+1), ]
#  yt <- yt[yt$zk == "50", ]
  #} else if (t$zk[1] == "25" && t$zk[2] == "91" && t$PS_code[1] == '_________' && t$NM_code[1] == '_________') {
  #  yt <- y[v:(v+1),] 
  #  yt <- yt[yt$zk == "11", ]
  } else if (t$zk[1] == "40" && t$zk[2] %in% c("40", "50", "53", "91")) {
      yt <- y[v, ] 
  } else if (t$zk[1] %in% c("41", "54", "51", "92") && t$zk[2] %in% c("41", "54", "51", "92")) {
    yt <- y[v, ] 
} else {stop("starpkodi2_vienadiDatumi: Trūkst izstrādes koda.")}

rm(y, t, prev, v)
return(yt)
}

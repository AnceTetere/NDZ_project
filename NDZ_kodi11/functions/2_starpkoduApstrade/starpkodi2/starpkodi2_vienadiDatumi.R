starpkodi2_vienadiDatumi <- function(y, t, prev, v) {
if ((t$zinkod[1] == "11" && t$zinkod[2] %in% c("40", "50", "53", "91")) ||
#    (t$zinkod[1] == "22" && t$zinkod[2] %in% c("51", "54")) ||
#    (t$zinkod[1] == "23" && t$zinkod[2] %in% c("41", "92")) ||   
#    (t$zinkod[1] == "24" && t$zinkod[2] %in% c("41", "51", "54")) ||   
    (t$zinkod[1] %in% c("21", "25") && t$zinkod[2] %in% c("41", "54", "51", "92"))) #||
#    (t$zinkod[1] == "40" && t$zinkod[2] == "51") ||
#    (t$zinkod[1] == "41" && t$zinkod[2] == "50") ||
#    (t$zinkod[1] == "51" && t$zinkod[2] == "53") ||
#    (t$zinkod[1] == "92" && t$zinkod[2] == "50")) 
{
  yt <- y[v,] 
  yt$dienas <- 0
#} else if (t$zinkod[1] == "21" && t$zinkod[2] == "53"){
#  yt <- y[v:(v + 1),]
#  yt <- yt[yt$zinkod == "11",]
} else if (t$zinkod[1] %in% c("21", "25", "29", "40") && t$zinkod[2] %in% c("50", "53")) {
  yt <- y[v:(v+1),] 
  yt <- yt[yt$zinkod %in% c("50", "53"), ] 
} else if (t$zinkod[1] == "11" && t$zinkod[2] %in% c("41", "51", "92")) {
  yt <- y[v, ]
  yt$dienas <- as.numeric(difftime(t$last_date[2], t$NDZ_sanemsanas_datums[1], units = "days")) + 1
#} else if (t$zinkod[1] == "50" && t$zinkod[2] == "53" && t$PS_code[1] == '__________' && t$NM_code[1] == '__________') {
#  yt <- y[v:(v+1), ]
#  yt <- yt[yt$zinkod == "50", ]
#} else if (t$zinkod[1] == "25" && t$zinkod[2] == "40") {
#  yt <- y[v:(v+1), ]
#  yt <- yt[yt$zinkod == "40", ]
#} else if (t$zinkod[1] == "50" && t$zinkod[2] == "53") {
#  yt <- y[v, ]
#  yt$dienas <- as.numeric(difftime(t$beidz_darbu[2], prev, units = "days")) - 1
#} else if (t$zinkod[1] == "51" && t$zinkod[2] == "54" &&
#           t$PS_code[1] == '__________' && t$NM_code[1] == '__________') {
#  yt <- y[v:(v+1), ]
#  yt <- yt[yt$zinkod == "50", ]
  #} else if (t$zinkod[1] == "25" && t$zinkod[2] == "91" && t$PS_code[1] == '__________' && t$NM_code[1] == '__________') {
  #  yt <- y[v:(v+1),] 
  #  yt <- yt[yt$zinkod == "11", ]
  #} else if (t$zinkod[1] == "40" && t$zinkod[2] == "50") {
  #  yt <- y[v, ] 
  #} else if (t$zinkod[1] == "41" && t$zinkod[2] == "51") {
  #  yt <- y[v, ] 
} else {stop("starpkodi2_vienadiDatumi: Trūkst izstrādes koda.")}

if(is.na(yt$PS_code[1])) {stop("Dienas NA.")}
return(yt)
}

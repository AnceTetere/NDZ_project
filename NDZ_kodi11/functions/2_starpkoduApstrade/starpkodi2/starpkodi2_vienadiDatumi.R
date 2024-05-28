starpkodi2_vienadiDatumi <- function(y2, t, prev, v) {
if ((t$zinkod[1] == "11" && t$zinkod[2] %in% c("40", "50", "53", "91")) ||
    (t$zinkod[1] == "21" && t$zinkod[2] %in% c("51", "41", "92")) ||
    (t$zinkod[1] == "22" && t$zinkod[2] == "51") ||
    (t$zinkod[1] == "23" && t$zinkod[2] %in% c("41", "92")) ||   
    (t$zinkod[1] == "24" && t$zinkod[2] %in% c("41", "51", "54")) ||   
    (t$zinkod[1] == "25" && t$zinkod[2] %in% c("41", "54", "51", "50", "92")) ||
    (t$zinkod[1] == "40" && t$zinkod[2] == "51") ||
    (t$zinkod[1] == "41" && t$zinkod[2] == "50") ||
    (t$zinkod[1] == "51" && t$zinkod[2] == "53") ||
    (t$zinkod[1] == "92" && t$zinkod[2] == "50")) {
  yt <- y2[v,] 
  yt$dienas <- 0
} else if (t$zinkod[1] == "21" && t$zinkod[2] == "53"){
  yt <- y2[v:(v + 1),]
  yt <- yt[yt$zinkod == "11",]
} else if (t$zinkod[1] == "21" && t$zinkod[2] == "50") {
  yt <- y2[v:(v+1),] 
  yt <- yt[yt$zinkod == "50", ] #jo ambiguity
} else if (t$zinkod[1] == "25" && t$zinkod[2] == "53") {
  yt <- y2[v:(v+1),] 
  yt <- yt[yt$zinkod == "53", ]
} else if (t$zinkod[1] == "25" && t$zinkod[2] == "91" && t$PS_code[1] == '_________' && t$NM_code[1] == '_________') {
  yt <- y2[v:(v+1),] 
  yt <- yt[yt$zinkod == "11", ]
} else if (t$zinkod[1] == "40" && t$zinkod[2] == "50") {
  yt <- y2[v, ] 
} else if (t$zinkod[1] == "41" && t$zinkod[2] == "51") {
  yt <- y2[v, ] 
} else if (t$zinkod[1] == "11" && t$zinkod[2] %in% c("41", "51", "92")) {
  yt <- y2[v, ]
  yt$dienas <- as.numeric(difftime(t$last_date[2], t$sak[1], units = "days"))
} else if (t$zinkod[1] == "50" && t$zinkod[2] == "53" && t$PS_code[1] == '_________' && t$NM_code[1] == '_________') {
  yt <- y2[v:(v+1), ]
  yt <- yt[yt$zinkod == "50", ]
} else if (t$zinkod[1] == "25" && t$zinkod[2] == "40") {
  yt <- y2[v:(v+1), ]
  yt <- yt[yt$zinkod == "40", ] #izvēlos kodu "40' jo tam nepieskaitās tā papildus darba diena. Pieņemu, ka indivīds jau ir aizgājis BK atvaļinājumā.
} else if (t$zinkod[1] == "50" && t$zinkod[2] == "53") {
  yt <- y2[v, ]
  yt$dienas <- as.numeric(difftime(t$beidz[2], prev, units = "days")) - 1
} else if (t$zinkod[1] == "51" && t$zinkod[2] == "54" &&
           t$PS_code[1] == '_________' && t$NM_code[1] == '_________') {
  yt <- y2[v:(v+1), ]
  yt <- yt[yt$zinkod == "50", ]
} else {
  stop("starpkodi2_vienadiDatumi: Trūkst izstrādes koda.")
}

if(is.na(yt$PS_code[1])) {stop("Dienas NA.")}
return(yt)
}

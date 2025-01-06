tripletkodi4 <- function(y3, t, prev, v) {
  
  yt <- y3[v, ]
  
  if (t$zk[1] %in% c("41", "51", "54", "92")) {
    if (t$zk[2] %in% c("40", "50", "53", "91")) {
      if (t$zk[3] %in% c("21", "22", "23", "24", "25", "29")) {
        if (t$zk[4] %in% c("41", "51", "54", "92")) {
          if(all(sapply(c(1,3), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) && diff(t$NDZ_sanemsanas_datums[2:3]) != 0) {    #Līdz ar bērna kopšanas atvaļinājuma beigšanos indivīds paņem bezalgas atvaļinājumu. 
            yt$dd <- 0
          } else {stop("Tripletkodi4 iztrūkst apstrādes koda.")}
        } else {stop("Tripletkodi4 iztrūkst apstrādes koda.")}
      } else {stop("Tripletkodi4 iztrūkst apstrādes koda.")}
    } else {stop("Tripletkodi4 iztrūkst apstrādes koda.")}
  } else if (t$zk[1] %in% c("40", "50", "53", "91")) {
    if (t$zk[2] %in% c("40", "50", "53", "91")) {
      if (t$zk[3] %in% c("21", "22", "23", "24", "25", "29")) {
        if (t$zk[4] %in% c("41", "51", "54", "92")) {
          if (diff(t$NDZ_sanemsanas_datums[1:2]) == 0 && all(diff(t$NDZ_sanemsanas_datums[2:4]) != 0)) {
            yt$dd <- as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1
          } else {stop("Tripletkodi4 iztrūkst apstrādes koda.")}
        } else {stop("Tripletkodi4 iztrūkst apstrādes koda.")}
      } else {stop("Tripletkodi4 iztrūkst apstrādes koda.")}
    } else {stop("Tripletkodi4 iztrūkst apstrādes koda.")}
  } else {stop("Tripletkodi4 iztrūkst apstrādes koda.")}

  if(is.na(yt$pseidokods[1])) {stop("DD NA.")}
  yt$zk <- "combined"  
  return(yt)
}

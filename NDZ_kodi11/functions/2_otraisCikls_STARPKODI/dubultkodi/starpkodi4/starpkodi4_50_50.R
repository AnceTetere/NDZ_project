starpkodi4_50_50 <- function(y, t, prev, v) {
  
  yt <- y[v,]
  
  if (t$zinkod[3] %in% c("41", "51", "54", "92")) {
    yt <- starpkodi4_50_50_51(y, t, prev, v)
  } else if (t$zinkod[3] %in% c("40", "50", "53", "91")) {
    if (t$zinkod[4] %in% c("41", "51", "54", "92")) {
      if(all(sapply(c(1,3), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) && diff(t$NDZ_sanemsanas_datums[2:3]) != 0) {
        #Te ir kļūda. Indivīds ar diviem kodiem (50 un 91) aiziet bezalgas bērna kopšanas atvaļinājumā.
        #Atgriežas ar vienu kodu (92), un aiziet nākamajā bērnu kopšanas atvaļinājumā.
        if (t$period[1] == "______" && t$PS_code[1] == 'PK82C8C9686' && t$NM_code[1] == '41502012170') {
          yt$dd <- as.numeric(sum(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "dd") - 1,
                                      diff(t$NDZ_sanemsanas_datums[3:4]) + 1))
        } else {stop("starpkodi4_50_50: Trūkst izstrādes koda.")}
      } else {stop("starpkodi4_50_50: Trūkst izstrādes koda.")}  
    } else {stop("starpkodi4_50_50: Trūkst izstrādes koda.")}
  } else if (t$zinkod[3] %in% c("21", "22", "23", "24", "25", "29")) {
    if (t$zinkod[4] %in% c("41", "51", "54", "92")) {
      if (all(diff(t$NDZ_sanemsanas_datums[1:3]) != 0) && diff(t$NDZ_sanemsanas_datums[3:4]) == 0) {
        #TE, TĀ KĀ, VISS KĀRTĪBĀ - BLOĶĒJU, JO PIRMOREIZ
        #Indivīds ar diviem kodiem aiziet bezalgas atvaļinājumā.
        #Līdz ar atgriešanos, tiek atlaists.
        if ((t$period[1] == "______" && t$PS_code[1] == 'PK80F4176A1' && t$NM_code[1] == '40103892068') ||
            (t$period[1] == "______" && t$PS_code[1] == 'PKCCF4BF970' && t$NM_code[1] == '40203345436')) {
          yt$dd <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[2], prev, units = "dd")) - 1,
                            as.numeric(diff(t$NDZ_sanemsanas_datums[3:4])))
        } else {stop("starpkodi4_50_50: Trūkst izstrādes koda.")}
      } else {stop("starpkodi4_50_50: Trūkst izstrādes koda.")}  
    } else {stop("starpkodi4_50_50: Trūkst izstrādes koda.")}
  } else {stop("starpkodi4_50_50: Trūkst izstrādes koda.")}    
  
  
  rm(y, t, prev, v)
  return(yt)
}

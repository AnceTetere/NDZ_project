starpkodi3_25_51 <- function(y, t, prev, v) {

  if (t$zk[3] %in% c("40", "50", "53", "91")) {
 #   if (all(diff(t$NDZ_sanemsanas_datums) == 0)) {
 #     yt <- y[v:(v+1), ]
 #     yt <- yt[yt$zk == "11", ] 
 #   } else
    if (diff(t$NDZ_sanemsanas_datums[1:2]) == 0 && diff(t$NDZ_sanemsanas_datums[2:3]) != 0) {
      if (t$PS_code[1] == '____________' && t$NM_code[1] == '____________') {
        yt <- y[v, ]
        yt$dd <- as.numeric(diff(t$NDZ_sanemsanas_datums[2:3]))
      } else {stop("Starpkodi3_25_51: Trūkst izstrādes koda.")}
    } else {stop("Starpkodi3_25_51: Trūkst izstrādes koda.")}
 } else if (t$zk[3] %in% c("41", "51", "54", "92")) {
           if (diff(t$NDZ_sanemsanas_datums[2:3]) != 0 && diff(t$NDZ_sanemsanas_datums[1:2]) == 0) {
             if (t$PS_code[1] == '____________' && t$NM_code[1] == '____________') {
                   t$zk[t$zk == '25'] <- '50'
                   yt <- y[v, ]
                   yt$dd <- sum(as.numeric(diff(t$NDZ_sanemsanas_datums[1:2])),
                                    as.numeric(difftime(t$last_date[3], t$NDZ_sanemsanas_datums[3], units = "days")) + 1)
             } else {
               yt <- y[v:(v+1), ]
               yt <- yt[yt$zk == "11", ]}
           } else if (all(diff(t$NDZ_sanemsanas_datums) == 0)) {
             if (t$PS_code[1] == '____________' && t$NM_code[1] == '____________') {
               yt <- y[v, ]
               yt$dd <- as.numeric(diff(t$NDZ_sanemsanas_datums[1:2]))
             } else {stop("Starpkodi3_25_51: Trūkst izstrādes koda.")}
           } else {stop("Starpkodi3_25_51: Trūkst izstrādes koda.")}
 } else if (t$zk[3] %in% c("11", "14", "16", "61")) {
            if (diff(t$NDZ_sanemsanas_datums[2:3]) != 0 && diff(t$NDZ_sanemsanas_datums[1:2]) == 0) {
              yt <- y[v, ]
              yt$dd <- as.numeric(difftime(t$last_date[3], t$NDZ_sanemsanas_datums[3], units = "days"))
            } else if (diff(t$NDZ_sanemsanas_datums[1:2]) == 0 && diff(t$NDZ_sanemsanas_datums[2:3]) != 0) {
             yt <- y[v, ]
             yt$dd <- as.numeric(difftime(t$last_date[3], t$NDZ_sanemsanas_datums[3], units = "days")) + 1 + 1 
           } else {stop("Starpkodi3_25_51: Trūkst izstrādes koda.")}
} else {stop("Starpkodi3_25_51: Trūkst izstrādes koda.")}
    
  rm(y, t, prev, v)
  return(yt)
}

starpkodi3_25_51 <- function(y, t, prev, v) {

  if (t$zinkod[3] == "50"){
 #   if (all(diff(t$NDZ_sanemsanas_datums) == 0)) {
 #     yt <- y[v:(v+1), ]
 #     yt <- yt[yt$zinkod == "11", ] 
 #   } else
    if (diff(t$NDZ_sanemsanas_datums[1:2]) == 0 && diff(t$NDZ_sanemsanas_datums[2:3]) != 0) {
      if (t$PS_code[1] == '_________' && t$NM_code[1] == '_________') {
        yt <- y[v, ]
        yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[2:3]))
      } else {stop("Starpkodi3_25_51: Trūkst izstrādes koda.")}
    } else {stop("Starpkodi3_25_51: Trūkst izstrādes koda.")}
 } else if (t$zinkod[3] %in% c("41", "51", "54", "92")) {
           if (diff(t$NDZ_sanemsanas_datums[2:3]) != 0 && diff(t$NDZ_sanemsanas_datums[1:2]) == 0) {
             if (t$PS_code[1] == '_________' && t$NM_code[1] == '_________') {
                   t$zinkod[t$zinkod == '25'] <- '50'
                   yt <- y[v, ]
                   yt$dienas <- sum(as.numeric(diff(t$NDZ_sanemsanas_datums[1:2])),
                                    as.numeric(difftime(t$last_date[3], t$NDZ_sanemsanas_datums[3], units = "days")) + 1)
             } else {
               yt <- y[v:(v+1), ]
               yt <- yt[yt$zinkod == "11", ]}
            } else {stop("Starpkodi3_25_51: Trūkst izstrādes koda.")}   
 } else if (t$zinkod[3] == "11") {
            if (diff(t$NDZ_sanemsanas_datums[2:3]) != 0 && diff(t$NDZ_sanemsanas_datums[1:2]) == 0) {
              yt <- y[v, ]
              yt$dienas <- as.numeric(difftime(t$last_date[3], t$NDZ_sanemsanas_datums[3], units = "days"))
            } else if (diff(t$NDZ_sanemsanas_datums[1:2]) == 0 && diff(t$NDZ_sanemsanas_datums[2:3]) != 0) {
             yt <- y[v, ]
             yt$dienas <- as.numeric(difftime(t$last_date[3], t$NDZ_sanemsanas_datums[3], units = "days")) + 1 + 1 
           } else {stop("Starpkodi3_25_51: Trūkst izstrādes koda.")}
} else {stop("Starpkodi3_25_51: Trūkst izstrādes koda.")}
    
  rm(y, t, prev, v)
  return(yt)
}

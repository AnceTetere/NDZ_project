starpkodi3_25_51 <- function(y, t, prev, v) {

  yt <- y[v, ]
  
  if (t$zinkod[3] %in% c("40", "50", "53", "91")) {
            if (all(diff(t$NDZ_sanemsanas_datums) == 0)) {
              if(t$period[1] == "_____" && t$PS_code[1] == '__________' && t$NM_code[1] == '__________') {
                     yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[2:3]))
              } else {stop("Starpkodi3_25_51: Trūkst izstrādes koda.")}
            } else if (diff(t$NDZ_sanemsanas_datums[1:2]) == 0 && diff(t$NDZ_sanemsanas_datums[2:3]) != 0) {
                 if (t$PS_code[1] == '__________' && t$NM_code[1] == '__________') {
                   yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[2:3]))
                 } else {stop("Starpkodi3_25_51: Trūkst izstrādes koda.")}
               } else {stop("Starpkodi3_25_51: Trūkst izstrādes koda.")}
 } else if (t$zinkod[3] %in% c("41", "51", "54", "92")) {
           if (diff(t$NDZ_sanemsanas_datums[2:3]) != 0 && diff(t$NDZ_sanemsanas_datums[1:2]) == 0) {
             if (t$PS_code[1] == '__________' && t$NM_code[1] == '__________') {
                   yt$dienas <- sum(as.numeric(diff(t$NDZ_sanemsanas_datums[1:2])),
                                    as.numeric(difftime(t$last_date[3], t$NDZ_sanemsanas_datums[3], units = "days")) + 1)
             } else if ((t$period[1] == '_____' && t$PS_code[1] == '__________' && t$NM_code[1] == '__________') ||
                        (t$period[1] == '_____' && t$PS_code[1] == '__________' && t$NM_code[1] == '__________')) {
               t$zinkod[t$zinkod == '51'][1] <- '50'
               t$sak_beidz[t$zinkod == '50'] <- '2'
               yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[3], prev, units = "days"))
             } else {
               yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[1:2]))
               ZERO_minus(t %>% slice(2))}
           } else if (all(diff(t$NDZ_sanemsanas_datums) == 0)) {
             if (year == "2022") {  # vienkārši, lai pārbaudītu ka citos gados šis arī izpildās - atstāju kopīgo visiem
               yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[1:2]))
               ZERO_minus(t %>% slice(3))
             } else {stop("Starpkodi3_25_51: Trūkst izstrādes koda.")}
           } else {stop("Starpkodi3_25_51: Trūkst izstrādes koda.")}
 } else if (t$zinkod[3] %in% c("11", "14", "16", "61")) {
            if (diff(t$NDZ_sanemsanas_datums[2:3]) != 0 && diff(t$NDZ_sanemsanas_datums[1:2]) == 0) {
              yt$dienas <- as.numeric(difftime(t$last_date[3], t$NDZ_sanemsanas_datums[3], units = "days"))
           } else {stop("Starpkodi3_25_51: Trūkst izstrādes koda.")}
} else {stop("Starpkodi3_25_51: Trūkst izstrādes koda.")}
    
  rm(y, t, prev, v)
  return(yt)
}

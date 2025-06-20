starpkodi3_25_51 <- function(y, t, prev, v) {

  yt <- y[v, ]
  
  if (t$zinkod[3] %in% c("40", "50", "53", "91")) {
            if (all(diff(t$ZDN_sanemsanas_datums) == 0)) {
              if ((t$period[1] == "______" && t$pseidokods[1] == "___________"   && t$nmrkod[1] == "___________" ) ||
                  (t$period[1] == "______" && t$pseidokods[1] == "___________"   && t$nmrkod[1] == "___________" ) ||
                  (t$period[1] == "______" && t$pseidokods[1] == "___________"   && t$nmrkod[1] == "___________" )) {
                     yt$dienas <- as.numeric(diff(t$ZDN_sanemsanas_datums[2:3]))
              } else {stop("Starpkodi3_25_51: Trūkst izstrādes koda.")}
            } else if (diff(t$ZDN_sanemsanas_datums[1:2]) == 0 && diff(t$ZDN_sanemsanas_datums[2:3]) != 0) {
                 if (t$pseidokods[1] == "___________"   && t$nmrkod[1] == "___________" ) {
                   yt$dienas <- as.numeric(diff(t$ZDN_sanemsanas_datums[2:3]))
                 } else {stop("Starpkodi3_25_51: Trūkst izstrādes koda.")}
               } else {stop("Starpkodi3_25_51: Trūkst izstrādes koda.")}
 } else if (t$zinkod[3] %in% c("41", "51", "54", "92")) {
           if (diff(t$ZDN_sanemsanas_datums[2:3]) != 0 && diff(t$ZDN_sanemsanas_datums[1:2]) == 0) {
             if (t$pseidokods[1] == "___________"   && t$nmrkod[1] == "___________" ) {
                   t$zinkod[t$zinkod == "25"] <- "50"
                   yt$dienas <- sum(as.numeric(diff(t$ZDN_sanemsanas_datums[1:2])),
                                    as.numeric(difftime(t$last_date[3], t$ZDN_sanemsanas_datums[3], units = "days")) + 1)
             } else if ((t$period[1] == "______"  && t$pseidokods[1] == "___________"   && t$nmrkod[1] == "___________" ) ||
                        (t$period[1] == "______"  && t$pseidokods[1] == "___________"   && t$nmrkod[1] == "___________" )) {
               t$zinkod[t$zinkod == "51"][1] <- "50"
               t$sak_beidz[t$zinkod == "50"] <- "2"
               yt$dienas <- as.numeric(difftime(t$ZDN_sanemsanas_datums[3], prev, units = "days"))
             } else {
               yt$dienas <- as.numeric(diff(t$ZDN_sanemsanas_datums[1:2]))
               ZERO_minus(t %>% slice(2))}
           } else if (all(diff(t$ZDN_sanemsanas_datums) == 0)) {
             if (year == "____") { 
               yt$dienas <- as.numeric(diff(t$ZDN_sanemsanas_datums[1:2]))
               ZERO_minus(t %>% slice(3))
             } else {stop("Starpkodi3_25_51: Trūkst izstrādes koda.")}
           } else {stop("Starpkodi3_25_51: Trūkst izstrādes koda.")}
 } else if (t$zinkod[3] %in% c("11", "14", "16", "61")) {
            if (diff(t$ZDN_sanemsanas_datums[2:3]) != 0 && diff(t$ZDN_sanemsanas_datums[1:2]) == 0) {
              yt$dienas <- as.numeric(difftime(t$last_date[3], t$ZDN_sanemsanas_datums[3], units = "days"))
           } else {stop("Starpkodi3_25_51: Trūkst izstrādes koda.")}
} else {stop("Starpkodi3_25_51: Trūkst izstrādes koda.")}
    
  rm(y, t, prev, v)
  return(yt)
}

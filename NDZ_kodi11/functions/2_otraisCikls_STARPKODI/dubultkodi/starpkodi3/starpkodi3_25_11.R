starpkodi3_25_11 <- function(y, t, prev, v) {
  
  yt <- y[v, ]
  
  if (t$zinkod[3] %in% c("40", "50", "53", "91")) {
         if (all(diff(t$NDZ_sanemsanas_datums) != 0))  {
           yt$dienas <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")),
                            as.numeric(diff(t$NDZ_sanemsanas_datums[2:3])))
         } else if (diff(t$NDZ_sanemsanas_datums[2:3]) == 0 && diff(t$NDZ_sanemsanas_datums[1:2]) != 0) {
           yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days"))
         } else if (all(diff(t$NDZ_sanemsanas_datums) == 0)) {
           if (t$period[1] == "_____" && t$PS_code[1] == "_____" && t$NM_code[1] == "_____") {
              yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[2:3]))
           } else {stop("Starpkodi3_25_11: Trūkst izstrādes koda.")}
         } else {stop("Starpkodi3_25_11: Trūkst izstrādes koda.")}
  } else if (t$zinkod[3] %in% c("41", "51", "54", "92")) {
          if (all(diff(t$NDZ_sanemsanas_datums) != 0))  {
            if(t$period[1] == "_____" && t$PS_code[1] == "_____" && t$NM_code[1] == "_____") {
              yt$dienas <- as.numeric(difftime(t$last_date[3], t$NDZ_sanemsanas_datums[3], units = "days")) + 1
            } else {stop("Starpkodi3_25_11: Trūkst izstrādes koda.")}
          } else {stop("Starpkodi3_25_11: Trūkst izstrādes koda.")}
  } else {stop("Starpkodi3_25_11: Trūkst izstrādes koda.")} 
  
  rm(y, t, prev, v)
  return(yt)
}

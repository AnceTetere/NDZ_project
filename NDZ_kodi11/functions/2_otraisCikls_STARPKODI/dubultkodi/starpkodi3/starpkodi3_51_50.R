starpkodi3_51_50 <- function(y, t, prev, v) {
  
  yt <- y[v, ]
  
if (t$zinkod[3] %in% c("21", "22", "23", "24", "25", "29")) {
         if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
           yt$dd <- as.numeric(diff(t$NDZ_sanemsanas_datums[1:2]))
         }  else if (diff(t$NDZ_sanemsanas_datums[1:2]) == 0 && diff(t$NDZ_sanemsanas_datums[2:3]) != 0) {
           yt$dd <- 0
         } else if (all(!diff(t$NDZ_sanemsanas_datums) == 0)) {
           yt$dd <- as.numeric(diff(t$NDZ_sanemsanas_datums[1:2]))
         } else {stop("Starpkodi3_51_50: Trūkst izstrādes koda.")}
} else if (t$zinkod[3] %in% c("40", "50", "53", "91")) {
        if (diff(t$NDZ_sanemsanas_datums[1:2]) != 0) {
          yt$dd <- as.numeric(diff(t$NDZ_sanemsanas_datums[2:3]))
        } else if (all(diff(t$NDZ_sanemsanas_datums) == 0)) {
          if (t$period[1] == "______" && t$PS_code[1] == '__________' && t$NM_code[1] == '__________') {
            yt$dd <- as.numeric(difftime(t$NDZ_sanemsanas_datums[3], prev, units = "dd"))
          } else {stop("Starpkodi3_51_50: Trūkst izstrādes koda.")}
        } else if (diff(t$NDZ_sanemsanas_datums[1:2]) == 0 && diff(t$NDZ_sanemsanas_datums[2:3]) != 0) {
          if (t$period[1] == "______" && t$PS_code[1] == '__________' && t$NM_code[1] == '__________') {
            yt$dd <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[2], prev, units = "dd")),
                             as.numeric(diff(t$NDZ_sanemsanas_datums[c(1,3)])))
          } else {stop("Starpkodi3_51_50: Trūkst izstrādes koda.")}
        } else {stop("Starpkodi3_51_50: Trūkst izstrādes koda.")}
} else if (t$zinkod[3] %in% c("41", "51", "54", "92")) {
       if (diff(t$NDZ_sanemsanas_datums[2:3]) != 0) {
         yt$dd <- sum(as.numeric(diff(t$NDZ_sanemsanas_datums[1:2])),
                          as.numeric(difftime(t$last_date[3], t$NDZ_sanemsanas_datums[3], units = "dd")) + 1)
       } else if (all(diff(t$NDZ_sanemsanas_datums) == 0)) {
         if(t$period[1] == "______" && t$PS_code[1] == "__________" && t$NM_code[1] == "__________") {
           yt$dd <- as.numeric(diff(t$NDZ_sanemsanas_datums[2:3]))
           ZERO_plus(t %>% slice(2))
         } else {stop("Starpkodi3_51_50: Trūkst izstrādes koda.")}
       } else if (diff(t$NDZ_sanemsanas_datums[1:2]) != 0 && diff(t$NDZ_sanemsanas_datums[2:3]) == 0) {
         if (t$period[1] == "______" && t$PS_code[1] == "__________" && t$NM_code[1] == "__________") {
           yt$dd <- as.numeric(diff(t$NDZ_sanemsanas_datums[1:2]))
         } else {stop("Starpkodi3_51_50: Trūkst izstrādes koda.")}
       } else {stop("Starpkodi3_51_50: Trūkst izstrādes koda.")}
} else {stop("Starpkodi3_51_50: Trūkst izstrādes koda.")}

rm(y, t, prev, v)
return(yt) 
} 

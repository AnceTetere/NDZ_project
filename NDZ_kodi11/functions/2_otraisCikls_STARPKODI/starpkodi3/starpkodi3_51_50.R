starpkodi3_51_50 <- function(y, t, prev, v) {
  
if (t$zk[3] %in% c("21", "22", "23", "24", "25", "29")){
         if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
           yt <- y2[v, ]
           yt$dd <- as.numeric(diff(t$NDZ_sanemsanas_datums[1:2]))
         }  else if (diff(t$NDZ_sanemsanas_datums[1:2]) == 0 && diff(t$NDZ_sanemsanas_datums[2:3]) != 0) {
           yt <- y2[v, ]
           yt$dd <- 0
         } else if (all(!diff(t$NDZ_sanemsanas_datums) == 0)) {
           yt <- y[v, ]
           yt$dd <- as.numeric(diff(t$NDZ_sanemsanas_datums[1:2]))
         } else {stop("Starpkodi3_51_50: Trūkst izstrādes koda.")}
} else if (t$zk[3] %in% c("40", "50", "53", "91")) {
        if (diff(t$NDZ_sanemsanas_datums[1:2]) != 0) {
          yt <- y[v, ]
          yt$dd <- as.numeric(diff(t$NDZ_sanemsanas_datums[2:3]))
        } else {stop("Starpkodi3_51_50: Trūkst izstrādes koda.")}
} else if (t$zk[3] %in% c("41", "51", "54", "92")) {
       if (diff(t$NDZ_sanemsanas_datums[2:3]) != 0) {
         yt <- y[v, ]
         yt$dd <- sum(as.numeric(diff(t$NDZ_sanemsanas_datums[1:2])),
                          as.numeric(difftime(t$last_date[3], t$NDZ_sanemsanas_datums[3], units = "days")) + 1)
       } else {stop("Starpkodi3_51_50: Trūkst izstrādes koda.")}
} else {stop("Starpkodi3_51_50: Trūkst izstrādes koda.")}

rm(y, t, prev, v)
return(yt) 
} 

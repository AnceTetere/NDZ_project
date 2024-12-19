starpkodi4_51 <- function(y, t, prev, v) {
   
  if (t$zk[2] %in% c("41", "51", "54", "92")) {
                if (t$zk[3] %in% c("40", "50", "53", "91")) {
                  if (t$zk[4] %in% c("21", "22", "23", "24", "25", "29")) {
                    if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
                      yt <- y[v:(v+1), ] %>% filter(zk == "50")
                    } else {stop("Starpkodi4_51: Trūkst izstrādes koda.")}
                  } else if (t$zk[4] %in% c("41", "51", "54", "92")) {
                    if (diff(t$NDZ_sanemsanas_datums[2:3]) == 0 && all(sapply(c(1,3), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
                      yt <- y[v,]
                      yt$dd <- as.numeric(sum(diff(t$NDZ_sanemsanas_datums[2:3]), 
                                                         difftime(t$last_date[4], t$NDZ_sanemsanas_datums[4], units = "days") + 1))
                    } else {stop("Starpkodi4_41 iztrūkst apstrādes koda.")}
                  } else {stop("Starpkodi4_41 iztrūkst apstrādes koda.")}
                } else if (t$zk[3] %in% c("21", "22", "23", "24", "25", "29")) {
                         if (t$zk[4] %in% c("21", "22", "23", "24", "25", "29"))  {
                          if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
                          yt <- y[v, ]
                          yt$dd <- as.numeric(diff(t$NDZ_sanemsanas_datums[c(2,4)]) + 1)
                    } else {stop("Starpkodi4_51: Trūkst izstrādes koda.")}
                  } else {stop("Starpkodi4_51: Trūkst izstrādes koda.")}
                } else {stop("Starpkodi4_51: Trūkst izstrādes koda.")}
  } else if (t$zk[2] %in% c("21", "22", "23", "24", "25", "29")) {
               if (t$zk[3] %in% c("41", "51", "54", "92")) {
                 if (t$zk[4] %in% c("41", "51", "54", "92")) {
                   if (all(sapply(c(1,3), function(i) all(diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) && diff(t$NDZ_sanemsanas_datums[2:3]) == 0) {
                     yt <- y[v, ]
                     yt$dd <- 0
                   } else {stop("Starpkodi4_51: Trūkst izstrādes koda.")}
                 } else {stop("Starpkodi4_51: Trūkst izstrādes koda.")}
               } else {stop("Starpkodi4_51: Trūkst izstrādes koda.")}
  } else if (t$zk[2] %in% c("40", "50", "53", "91")) {
            yt <- starpkodi4_51_50(y, t, prev, v)
  } else {stop("Starpkodi4_51: Trūkst izstrādes koda.")}

  rm(y, t, prev, v)
  return(yt) 
}

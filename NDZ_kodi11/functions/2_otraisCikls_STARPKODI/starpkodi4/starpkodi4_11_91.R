starpkodi4_11_91 <- function(y, t, prev, v) {
  
  if (t$zk[3] %in% c("41", "51", "54", "92")) {
         if (t$zk[4] %in% c("41", "51", "54", "92")) {
           if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
             yt <- y[v, ]
             yt$dd <- as.numeric(sum(diff(t$NDZ_sanemsanas_datums[1:2]),
                                     difftime(t$last_date[4], t$NDZ_sanemsanas_datums[4], units = "days")))
           } else {stop("Starpkodi4_11_91: Trūkst izstrādes koda.")}
         } else if (t$zk[4] %in% c("40", "50", "53", "91")) {
           if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
             yt <- y[v, ]
             yt$dd <- as.numeric(sum(diff(t$NDZ_sanemsanas_datums[1:2]), diff(t$NDZ_sanemsanas_datums[3:4])))
           } else {stop("Starpkodi4_11_91: Trūkst izstrādes koda.")}
         } else {stop("Starpkodi4_11_91: Trūkst izstrādes koda.")} 
 # } else if (t$zk[3] == "25") {
 #       if (t$zk[4] == "92") {
 #         if (all(sapply(seq(1,4,by=2), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) && 
 #           yt <- y[v, ]
 #           yt$dd <- 0
 #         } else {stop("Starpkodi4_11_91: Trūkst izstrādes koda.")}
 #       } else {stop("Starpkodi4_11_91: Trūkst izstrādes koda.")}
  } else if (t$zk[3] %in% c("40", "50", "53", "91")) {
              if (t$zk[4] %in% c("41", "51", "54", "92")) {
                if (all(sapply(c(1,3), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0)) &&
                    diff(t$NDZ_sanemsanas_datums[2:3]) == 0) {
                    if (t$period[1] == '202201' && t$PS_code[1] == '____________' && t$NM_code[1] == '____________') {
                      yt <- y[v, ]
                      yt$dd <- as.numeric(sum(diff(t$NDZ_sanemsanas_datums[c(1,3)]), 
                                                  difftime(t$last_date[4], t$NDZ_sanemsanas_datums[4], units = "days") + 1))
                    } else {stop("Starpkodi4_11_91: Trūkst izstrādes koda.")}
                } else if (all(diff(t$NDZ_sanemsanas_datums[1:3]) == 0) &&
                           diff(t$NDZ_sanemsanas_datums[3:4]) != 0) {
                  if (t$period[1] == "202201" && t$PS_code[1] == '____________' && t$NM_code[1] == '____________') {
				  yt <- y[v, ]
                  yt$dd <- as.numeric(sum(diff(t$NDZ_sanemsanas_datums[c(1,3)]),
                                              difftime(t$last_date[4], t$NDZ_sanemsanas_datums[4], units = "days")) + 1) 
				  } else {stop("Starpkodi4_11_91: Trūkst izstrādes koda.")}
                  
                } else {stop("Starpkodi4_11_91: Trūkst izstrādes koda.")}
              } else {stop("Starpkodi4_11_91: Trūkst izstrādes koda.")}
  } else {stop("Starpkodi4_11_91: Trūkst izstrādes koda.")}
  
  rm(y, t, prev, v)
  return(yt) 
}

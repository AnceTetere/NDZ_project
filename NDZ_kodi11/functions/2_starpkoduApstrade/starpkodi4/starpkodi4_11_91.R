starpkodi4_11_91 <- function(y, t, prev, v) {
  
  if (t$zinkod[3] == "92") {
         if (t$zinkod[4] == "92") {
           if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
             yt <- y[v, ]
             yt$dienas <- sum(as.numeric(diff(t$NDZ_sanemsanas_datums[1:2])), 
                              as.numeric(difftime(t$last_date[4], t$NDZ_sanemsanas_datums[4], units = "days")))
           } else {stop("Starpkodi4_11_91: Trūkst izstrādes koda.")}
        # } else if (t$zinkod[4] == "91") {
        #   if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
        #     yt <- y[v, ]
        #     yt$dienas <- sum(as.numeric(diff(t$NDZ_sanemsanas_datums[1:2])), as.numeric(diff(t$NDZ_sanemsanas_datums[3:4])))
        #   } else {stop("Starpkodi4_11_91: Trūkst izstrādes koda.")}
         } else {stop("Starpkodi4_11_91: Trūkst izstrādes koda.")} 
 # } else if (t$zinkod[3] == "25") {
 #       if (t$zinkod[4] == "92") {
 #         if (all(sapply(seq(1,4,by=2), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) && 
 #             t$NDZ_sanemsanas_datums[2:3] != 0) {#Dienā, kad cilvēks uzsāk darbu, tas aiziet bērna kopšanas atvaļinājumā.
 #           yt <- y[v, ]
 #           yt$dienas <- 0
 #         } else {stop("Starpkodi4_11_91: Trūkst izstrādes koda.")}
 #       } else {stop("Starpkodi4_11_91: Trūkst izstrādes koda.")}
  } else {stop("Starpkodi4_11_91: Trūkst izstrādes koda.")}
  
  rm(y, t, prev, v)
  return(yt) 
}

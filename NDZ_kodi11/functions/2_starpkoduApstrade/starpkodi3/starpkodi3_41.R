starpkodi3_41 <- function(y, t, prev, v) {

  if (t$zinkod[2] == "41") {
        if (t$zinkod[3] == "25") {
          if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
            yt <- y[v, ]
            yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[2:3]))
          } else {stop("Starpkodi3_41: Trūkst izstrādes koda.")}
    } else if (t$zinkod[3] == "50") {
        if (diff(t$NDZ_sanemsanas_datums[1:2]) != 0 && diff(t$NDZ_sanemsanas_datums[2:3]) == 0) {
          yt <- y[v, ]
          yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[2:3]))
        } else {stop("Starpkodi3_41: Trūkst izstrādes koda.")}
      } else {stop("Starpkodi3_41: Trūkst izstrādes koda.")}
  } else {stop("Starpkodi3_41: Trūkst izstrādes koda.")}
  
    
#  if (t$zinkod[2] == "50" &&
#           t$zinkod[3] == "51" && all(!diff(t$NDZ_sanemsanas_datums[2:3]) == 0) &&
#           t$NDZ_sanemsanas_datums[1] == t$NDZ_sanemsanas_datums[2]) {
#    yt <- y[v, ]
#    yt$dienas <- as.numeric(difftime(t$last_date[3], t$NDZ_sanemsanas_datums[3], units = "days")) + 1 
#  } else if (t$zinkod[2] == "50" && t$zinkod[3] == "51" && 
#             all(diff(t$NDZ_sanemsanas_datums) != 0)) {
#    days1 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[2], t$NDZ_sanemsanas_datums[1], units = "days")) 
#    days2 <- as.numeric(difftime(t$last_date[3], t$NDZ_sanemsanas_datums[3], units = "days"))  + 1 
    
#    yt <- y[v, ]
#    yt$dienas <- sum(days1, days2)
#    rm(days1, days2)
#  } else if (t$zinkod[2] == "92" && t$zinkod[3] == "25" && 
#             all(diff(t$NDZ_sanemsanas_datums[2:3]) != 0) && all(diff(t$NDZ_sanemsanas_datums[1:2]) == 0)) {
#    yt <- y[v, ]
#    yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[3], t$NDZ_sanemsanas_datums[1], units = "days"))
#  } else if (t$zinkod[2] == "51" && t$zinkod[3] == "50" && all(diff(t$NDZ_sanemsanas_datums) == 0) &&
#             t$PS_code[1] == '____________' && t$NM_code[1] == '____________') {
#    yt <- y[v, ]
#    yt$dienas <- as.numeric(difftime(t$last_date[1], t$NDZ_sanemsanas_datums[1], units = "days")) + 1 
#  } else if (t$zinkod[2] == "53" && t$zinkod[3] == "54" && diff(t$NDZ_sanemsanas_datums[2:3]) != 0 && 
#             diff(t$NDZ_sanemsanas_datums[1:2]) == 0) {
#    yt <- y[v, ]
#    yt$dienas <- as.numeric(difftime(t$last_date[3], t$NDZ_sanemsanas_datums[3], units = "days")) + 1
#  } else if (t$zinkod[2] == "25" && t$zinkod[3] == "41" && diff(t$NDZ_sanemsanas_datums[2:3]) == 0 && 
#             diff(t$NDZ_sanemsanas_datums[1:2]) != 0) {
#    yt <- y[v, ]
#    yt$dienas <- 0
  
  return(yt) 
}

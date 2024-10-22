starpkodi3_11_50 <- function(y, t, prev, v) {

  if (t$zinkod[3] == "51") {
    if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
      yt <- y[v, ]
      yt$dienas <- sum(as.numeric(diff(t$NDZ_sanemsanas_datums[1:2])), 
                       as.numeric(difftime(t$last_date[3], t$NDZ_sanemsanas_datums[3], units = "days")) + 1)
    } else if (diff(t$NDZ_sanemsanas_datums[1:2]) == 0 && diff(t$NDZ_sanemsanas_datums[2:3]) != 0) {
      yt <- y[v, ]
      yt$dienas <- as.numeric(difftime(t$last_date[3], t$NDZ_sanemsanas_datums[3], units = "days")) + 1 # jo darba sākšana
    } else {stop("Starpkodi3_11_50: Trūkst izstrādes koda.")}
  } else {stop("Starpkodi3_11_50: Trūkst izstrādes koda.")}
  
    
#   else if (t$zinkod[3] == "25" && all(!diff(t$NDZ_sanemsanas_datums) == 0)) {
#    yt <- y[v, ]
#    yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[2], t$NDZ_sanemsanas_datums[1], units = "days"))
#  } else if (t$zinkod[3] == "25" && all(!diff(t$NDZ_sanemsanas_datums) == 0)) {
#    yt <- y[v, ]
#    yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[2], t$NDZ_sanemsanas_datums[1], units = "days"))
#  } else if (t$zinkod[3] == "50" && all(diff(t$NDZ_sanemsanas_datums) != 0)) {
#    yt <- y[v, ]
#    yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[3], t$NDZ_sanemsanas_datums[1], units = "days"))
#  } else if (t$zinkod[3] == "21" && all(!diff(t$NDZ_sanemsanas_datums) == 0)) {
#    yt <- y[v, ]
#    yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[2], t$NDZ_sanemsanas_datums[1], units = "days"))
#  } else if (t$zinkod[3] == "21" && diff(t$NDZ_sanemsanas_datums[1:2]) == 0 && 
#             diff(t$NDZ_sanemsanas_datums[2:3]) != 0) {
#    yt <- y[v, ]
#    yt$dienas <- 0
#  } else if (t$zinkod[3] == "11" && t$PS_code[1] == '______________' && t$NM_code[1] == '______________') {
#    yt <- y[v, ]
#    yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[2], t$NDZ_sanemsanas_datums[1], units = "days"))
#  } else if (t$zinkod[3] == "25") {
#    yt <- y[v, ]
#    yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[2], t$NDZ_sanemsanas_datums[1], units = "days"))
#  } else if (t$zinkod[3] == "50" && diff(t$NDZ_sanemsanas_datums[1:2]) == 0 && diff(t$NDZ_sanemsanas_datums[2:3]) != 0 &&
#             t$PS_code[1] == '______________' && t$NM_code[1] == '______________') {
#    yt <- y[v, ]
#    yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[3], t$NDZ_sanemsanas_datums[1], units = "days"))


  
  return(yt) 
}

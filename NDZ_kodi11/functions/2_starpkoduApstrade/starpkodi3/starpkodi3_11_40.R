starpkodi3_11_40 <- function(y, t, prev, v) {
  
  if (t$zinkod[3] == "41") {
    if (diff(t$NDZ_sanemsanas_datums[2:3]) != 0) {
      yt <- y[v, ]
      yt$dienas <- sum(as.numeric(diff(t$NDZ_sanemsanas_datums[1:2])),
                       as.numeric(difftime(t$last_date[3], t$NDZ_sanemsanas_datums[3], units = "days")) + 1)
    } else {stop("Starpkodi3_11_40: Trūkst izstrādes koda.")}
    
# } else if (t$zinkod[3] == "40"){
#   if (t$PS_code[1] == '_____________' && t$NM_code[1] == '_____________') {
#     yt <- y[v, ]
#     yt$dienas <- sum(as.numeric(diff(t$NDZ_sanemsanas_datums[1:2])), 
#                      as.numeric(difftime(t$last_date[3], t$NDZ_sanemsanas_datums[3], units = "days")) + 1)
#   } else if (diff(t$NDZ_sanemsanas_datums[1:2]) != 0 && diff(t$NDZ_sanemsanas_datums[2:3]) == 0) {
#     yt <- y[v, ]
#     yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[c(1,3)])
#   } else {stop("Starpkodi3_11: Trūkst izstrādes koda.")}
# } else if (t$zinkod[3] == "91") {
#   if (all(diff(t$NDZ_sanemsanas_datums) == 0)) {
#     yt <- y[v, ]
#     yt$dienas <- 0
#   } else {stop("Starpkodi3_11: Trūkst izstrādes koda.")}
# } else if (t$zinkod[3] == "25"){
#   if(diff(t$NDZ_sanemsanas_datums[1:2]) == 0 && diff(t$NDZ_sanemsanas_datums[2:3]) != 0) {
#     yt <- y[v, ]
#     yt$dienas <- 0
#   } else {stop("Starpkodi3_11: Trūkst izstrādes koda.")}
  } else {stop("Starpkodi3_11: Trūkst izstrādes koda.")}
  
  
  return(yt)
}

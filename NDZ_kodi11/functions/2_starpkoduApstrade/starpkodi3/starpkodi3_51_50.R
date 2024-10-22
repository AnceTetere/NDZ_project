starpkodi3_51_50 <- function(y, t, prev, v) {
  
    if (t$zinkod[3] == "21") {
         if (all(!diff(t$NDZ_sanemsanas_datums) == 0)) {
             yt <- y[v, ]
             yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[1:2]))
         } else {stop("Starpkodi3_51_50: Trūkst izstrādes koda.")}
    } else if (t$zinkod[3] == "25") {
         if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
           yt <- y2[v, ]
           yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[1:2]))
         } else {stop("Starpkodi3_51_50: Trūkst izstrādes koda.")}
    } else {stop("Starpkodi3_51_50: Trūkst izstrādes koda.")}

if(is.na(yt$PS_code[1])) {stop("Dienas NA.")}
return(yt) 
} 

#}  else if (t$zinkod[3] == "21" && all(diff(t$NDZ_sanemsanas_datums[1:2]) == 0) && all(diff(t$NDZ_sanemsanas_datums[2:3]) != 0)) {
#    yt <- y2[v, ]
#    yt$dienas <- 0
#}else if (t$zinkod[3] == "53" && all(diff(t$NDZ_sanemsanas_datums) == 0)) {
#  yt <- y2[v, ]
#  yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[3], prev, units = "days")) - 1  
#

#} else 

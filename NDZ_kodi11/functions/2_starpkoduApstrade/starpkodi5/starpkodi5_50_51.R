starpkodi5_50_51 <- function(y, t, prev, v) {
  
  if (t$zinkod[3] == "50") {
    yt <- starpkodi5_50_51_50(y, t, prev, v)
  } else if (t$zinkod[3] == "51") {
         if (t$zinkod[4] == "21") {
         if (t$zinkod[5] == "50") {
         if (all(sapply(c(1,4), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0) & 
             diff(t$NDZ_sanemsanas_datums[2:4]) != 0)) {
           if (t$PS_code[1] == '__________' && t$NM_code[1] == '__________') {
             yt <- y[v,]
             yt$dienas <- sum(as.numeric(diff(t$NDZ_sanemsanas_datums[1:2])), 
                              as.numeric(diff(t$NDZ_sanemsanas_datums[3:4])))
           } else {stop("Starpkodi5_50_51: iztrūkst apstrādes koda.")}
         } else {stop("Starpkodi5_50_51: iztrūkst apstrādes koda.")}
         } else {stop("Starpkodi5_50_51: iztrūkst apstrādes koda.")}
         } else {stop("Starpkodi5_50_51: iztrūkst apstrādes koda.")} 
#   } else if(t$zinkod[3] == "25") {
#     if (t$zinkod[4] == "51") {
#       if (t$zinkod[5] == "11") {
#         if (all(sapply(c(1,3), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) && diff(t$NDZ_sanemsanas_datums[4:5]) != 0) {
#           yt <- y[v, ]
#           yt$dienas <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1, #jo atvaļinājums 
#                            as.numeric(difftime(t$last_date[5], t$NDZ_sanemsanas_datums[5], units = "days")) + 1) #jo darbs
#         } else {stop("Starpkodi5_50_51: iztrūkst apstrādes koda.")}
#       } else {stop("Starpkodi5_50_51: iztrūkst apstrādes koda.")} 
#     } else {stop("Starpkodi5_50_51: iztrūkst apstrādes koda.")} 
#  } else if(t$zinkod[3] == "91") {
#    if (t$zinkod[4] == "92") {
#      if (t$zinkod[5] == "40") {
#        if (all(diff(t$NDZ_sanemsanas_datums[1:4]) != 0) && diff(t$NDZ_sanemsanas_datums[4:5]) == 0) {
#          yt <- y[v, ]
#          yt$dienas <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1, #jo atvaļinājums
#                           as.numeric(diff(t$NDZ_sanemsanas_datums[2:3])),
#                           as.numeric(diff(t$NDZ_sanemsanas_datums[4:5])))
#        } else {stop("Starpkodi5_50_51: iztrūkst apstrādes koda.")}
#      } else {stop("Starpkodi5_50_51: iztrūkst apstrādes koda.")}
#    } else {stop("Starpkodi5_50_51: iztrūkst apstrādes koda.")}
  } else {stop("Starpkodi5_50_51: iztrūkst apstrādes koda.")}

  
  return(yt)
}

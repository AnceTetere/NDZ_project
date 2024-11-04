starpkodi4 <- function(y, t, prev, v) {
  
if (t$zinkod[1] == "11") {
    yt <- starpkodi4_11(y, t, prev, v)
 } else if (t$zinkod[1] == "40") {
    yt <- starpkodi4_40(y, t, prev, v)
 } else if (t$zinkod[1] == "41") {
   yt <- starpkodi4_41(y, t, prev, v)
 } else if (t$zinkod[1] == "50") {
    yt <- starpkodi4_50(y, t, prev, v)
 } else if (t$zinkod[1] == "51") {
    yt <- starpkodi4_51(y, t, prev, v)
 } else if (t$zinkod[1] %in% c("21", "22", "23", "24", "25", "29")) {
   yt <- starpkodi4_25(y, t, prev, v)
    
   
# } else if (t$zinkod[1] == "53") {
#    yt <- starpkodi4_53(y, t, prev, v)
# } else if (t$zinkod[1] == "91") {
#   yt <- starpkodi4_91(y, t, prev, v)
#  } else if (t$zinkod[1] == "92" && t$zinkod[2] == "91" &&  t$zinkod[3] == "92" && t$zinkod[4] == "25" && 
#      all(diff(t$NDZ_sanemsanas_datums) != 0)) {
#    days1 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[2], t$NDZ_sanemsanas_datums[1], units = "days")) 
#    days2 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[4], t$NDZ_sanemsanas_datums[3], units = "days")) + 1 
#
#    yt <- y[v, ]
#    yt$dienas <- sum(days1, days2)
#    rm(days1, days2)
#  } else  else if (t$zinkod[1] == "26" && t$zinkod[2] == "11" &&  t$zinkod[3] == "11" && t$zinkod[4] == "50" && 
#             all(diff(t$NDZ_sanemsanas_datums[1:2]) == 0) && all(diff(t$NDZ_sanemsanas_datums[2:4]) != 0)) {
#    yt <- y[v, ]
#    yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[4], t$NDZ_sanemsanas_datums[3], units = "days"))
#  } else if (t$zinkod[1] == "92" && t$zinkod[2] == "91" &&  t$zinkod[3] == "92" && t$zinkod[4] == "50" && 
#             all(diff(t$NDZ_sanemsanas_datums[3:4]) == 0) && all(diff(t$NDZ_sanemsanas_datums[1:3]) != 0)) {
#    yt <- y[v, ]
#    yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[2], t$NDZ_sanemsanas_datums[1], units = "days"))
#  } else if (t$zinkod[1] == "92" && t$zinkod[2] == "91" &&  t$zinkod[3] == "25" && t$zinkod[4] == "92" && 
#             all(diff(t$NDZ_sanemsanas_datums[3:4]) == 0) && all(diff(t$NDZ_sanemsanas_datums[1:3]) != 0)) {
#    yt <- y[v, ]
#    yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[2], t$NDZ_sanemsanas_datums[1], units = "days"))
#  } else if (t$zinkod[1] == "92" && t$zinkod[2] == "91" &&  t$zinkod[3] == "25" && t$zinkod[4] == "92" && 
#             all(diff(t$NDZ_sanemsanas_datums[3:4]) == 0) && all(diff(t$NDZ_sanemsanas_datums[1:3]) != 0)) {
#    yt <- y[v, ]
#    yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[2], t$NDZ_sanemsanas_datums[1], units = "days"))

    
 } else {stop("Starpkodi4 iztrūkst apstrādes koda.")}
  
  if(is.na(yt$PS_code[1])) {stop("Dienas NA.")}
  yt$zinkod <- "combined"  #jo starpkodu dienu sarēķins
  return(yt)
}

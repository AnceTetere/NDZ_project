starpkodi4 <- function(y, t, prev, v) {
  
if (t$zk[1] %in% c("11", "14", "16", "61")) {
    yt <- starpkodi4_11(y, t, prev, v)
 } else if (t$zk[1] %in% c("40", "50", "53", "91")) {
    yt <- starpkodi4_50(y, t, prev, v)
 } else if (t$zk[1] %in% c("41", "51", "54", "92")) {
    yt <- starpkodi4_51(y, t, prev, v)
 } else if (t$zk[1] %in% c("21", "22", "23", "24", "25", "29")) {
   yt <- starpkodi4_25(y, t, prev, v)
    
   
# } else if (t$zk[1] == "53") {
#    yt <- starpkodi4_53(y, t, prev, v)
# } else if (t$zk[1] == "91") {
#   yt <- starpkodi4_91(y, t, prev, v)
#  } else  else if (t$zk[1] == "26" && t$zk[2] == "11" &&  t$zk[3] == "11" && t$zk[4] == "50" && 
#             all(diff(t$NDZ_sanemsanas_datums[1:2]) == 0) && all(diff(t$NDZ_sanemsanas_datums[2:4]) != 0)) {
#    yt <- y[v, ]
#    yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[4], t$NDZ_sanemsanas_datums[3], units = "days"))

    
 } else {stop("Starpkodi4 iztrūkst apstrādes koda.")}
  
  if(is.na(yt$PS_code[1])) {stop("Dienas NA.")}
  yt$zk <- "combined"  #jo starpkodu dienu sarēķins
  return(yt)
}

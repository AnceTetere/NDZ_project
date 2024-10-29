starpkodi3_11 <- function(y, t, prev, v) {
  
  if (t$zinkod[2] == "16"){
    yt <- starpkodi3_11_16(y, t, prev, v)
  } else if (t$zinkod[2] == "29"){
    yt <- starpkodi3_11_29(y, t, prev, v)
  } else if (t$zinkod[2] %in% c("40", "50", "53", "91")) {
    yt <- starpkodi3_11_50(y, t, prev, v)
  } else {stop("Starpkodi3_11: Trūkst izstrādes koda.")}
  
  rm(y, t, prev, v)
  return(yt) 
}    
  
  
#  } else if (t$zinkod[2] %in% c("21", "25")) {
#    yt <- starpkodi3_11_25(y, t, prev, v)
#  } else if (t$zinkod[2] %in% c("51", "41")) {
#    yt <- starpkodi3_11_51(y, t, prev, v)
#  } else 
  
#  } else if (t$zinkod[2] == "11" && t$zinkod[3] == "91" && all(diff(t$NDZ_sanemsanas_datums) != 0)) {
#    yt <- y[v, ]
#    yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[3], t$NDZ_sanemsanas_datums[2], units = "days"))
# 

#  } else if (t$zinkod[2] == "11" && t$zinkod[3] == "50" && all(diff(t$NDZ_sanemsanas_datums) != 0)) {
#    yt <- y[v, ]
#    yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[3], t$NDZ_sanemsanas_datums[2], units = "days"))
#  } else if (t$zinkod[2] == "11" && t$zinkod[3] %in% c("40", "50")) {
#    yt <- y[v, ]
#    yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[3], t$NDZ_sanemsanas_datums[2], units = "days"))


starpkodi8 <- function(y, t, prev, v) {

  yt <- y[v, ] 
  
  if (all(t$zinkod[1:8] == c("11", "50", "51", "50", "51", "50", "51", "50")) && 
        all(diff(t$NDZ_sanemsanas_datums) != 0)) {
    yt$dienas <- sum(sapply(seq(1,8,by=2), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)])))
  } else if (all(t$zinkod[1:8] == c("50", "51", "50", "51", "50", "51", "50", "25")) && 
             all(diff(t$NDZ_sanemsanas_datums) != 0)) {
    yt$dienas <- as.numeric(sum(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days") - 1, 
                                diff(t$NDZ_sanemsanas_datums[2:3]), diff(t$NDZ_sanemsanas_datums[4:5]),
                                diff(t$NDZ_sanemsanas_datums[6:7])))
  } else if (all(t$zinkod[1:8] == c("21", "11", "50", "51", "50", "51", "50", "51")) && 
             diff(t$NDZ_sanemsanas_datums[4:5]) == 0 &&
             all(sapply(c(1,2,5,6,7), function(i) t$NDZ_sanemsanas_datums[i:(i+1)] != 0))) {
    yt$dienas <- as.numeric(sum(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days"),
                                diff(t$NDZ_sanemsanas_datums[2:3]), diff(t$NDZ_sanemsanas_datums[4:5]),
                                diff(t$NDZ_sanemsanas_datums[6:7]), difftime(t$last_date[8], t$NDZ_sanemsanas_datums[8], units = "days")))
  } else if (all(t$zinkod[1:8] == c("11", "53", "54", "53", "54", "53", "54", "53")) && 
             all(diff(t$NDZ_sanemsanas_datums[3:4]) == 0) &&
             all(diff(t$NDZ_sanemsanas_datums[7:8]) == 0) && 
             all(diff(t$NDZ_sanemsanas_datums[1:3]) != 0) &&
             all(diff(t$NDZ_sanemsanas_datums[5:7]) != 0)) {
    yt$dienas <- sum(sapply(seq(1,8,by=2), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)])))
  } else if (all(t$zinkod[1:8] == c("25", "11", "11", "50", "25", "51", "25", "11")) && 
             all(sapply(seq(1, nrow(t), by = 2), function(i) all(diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0))) &&
             t$PS_code[1] == '_________' && t$NM_code[1] == '_________') {
    yt$dienas <- as.numeric(sum(diff(t$NDZ_sanemsanas_datums[1:2]) + 1, #jo darbs
                                diff(t$NDZ_sanemsanas_datums[7:8]) + 1)) #jo darbs
  } else if (all(t$zinkod[c(1,4,6)] == "50") && all(t$zinkod[c(2,3,5,7)] == "51") && t$zinkod[8] == "25" && 
              all(diff(t$NDZ_sanemsanas_datums) != 0)) {
    yt$dienas <- as.numeric(sum(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days") - 1, 
                                diff(t$NDZ_sanemsanas_datums[3:4]), diff(t$NDZ_sanemsanas_datums[5:6]),
                                diff(t$NDZ_sanemsanas_datums[7:8]) + 1)) # jo darbs
  } else if (t$zinkod[1] == "11" && all(t$zinkod[c(2,4,6)] == "50") && all(t$zinkod[c(3,5,7)] == "51") && t$zinkod[8] == "21" && 
             all(diff(t$NDZ_sanemsanas_datums) != 0)) {
    days <- 0
    for (d in seq(1,7,by=2)) {
      days <- days + as.numeric(difftime(t$NDZ_sanemsanas_datums[d+1], t$NDZ_sanemsanas_datums[d], units = "days"))
    }
    
    yt$dienas <- days + 1
    rm(days, d)
  } else if (all(t$zinkod[c(1,5,7)] == "50") && all(t$zinkod[c(2,6,8)] == "51") && t$zinkod[3] == "53" && t$zinkod[4] == "54" &&
             all(diff(t$NDZ_sanemsanas_datums) != 0)) {
    days <- as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1
    for (d in seq(2,6,by=2)) {days <- days + as.numeric(difftime(t$NDZ_sanemsanas_datums[d+1], t$NDZ_sanemsanas_datums[d], units = "days"))}
    days <- days + as.numeric(difftime(t$last_date[8], t$NDZ_sanemsanas_datums[8], units = "days")) + 1
    
    yt$dienas <- days
    rm(days, d)
  } else if (all(t$zinkod[seq(2,8,by=2)] == "50") && all(t$zinkod[seq(3,8,by=2)] == "51") && t$zinkod[1] == "11") {
    yt$dienas <- sum(sapply(seq(1,8,by=2), function(i) as.numeric(difftime(t$NDZ_sanemsanas_datums[i+1], t$NDZ_sanemsanas_datums[i], units = "days"))))
  } else if (all(t$zinkod[c(1,3,5)] == "91") && all(t$zinkod[c(2,4,6)] == "92") && all(t$zinkod[7:8] == c("50", "51"))) {
    if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
      yt$dienas <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1,
                       sapply(c(2,4,6), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)])),
                       as.numeric(difftime(t$last_date[8], t$NDZ_sanemsanas_datums[8], units = "days")))
    } else {stop("Starpkodi8 iztrūkst apstrādes koda.")} 
  } else if (t$zinkod[1] %in% c("40", "50", "53", "91")) {
    starpkodi8_50(y, t, prev, v)
  } else {stop("Starpkodi8 iztrūkst apstrādes koda.")}
  
  if(is.na(yt$PS_code[1])) {stop("Dienas NA.")}
  yt$zinkod <- "combined"  
  return(yt)
}

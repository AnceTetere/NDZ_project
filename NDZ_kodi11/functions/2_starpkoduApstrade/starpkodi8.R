starpkodi8 <- function(y2, t, prev, v) {

    if (all(t$zk[1:8] == c("11", "50", "51", "50", "51", "50", "51", "50")) && 
        all(diff(t$NDZ_sanemsanas_datums) != 0)) {
    yt <- y2[v, ]
    yt$dd <- sum(sapply(seq(1,8,by=2), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)])))
  } else if (all(t$zk[1:8] == c("50", "51", "50", "51", "50", "51", "50", "25")) && 
             all(diff(t$NDZ_sanemsanas_datums) != 0)) {
    yt <- y2[v, ]
    yt$dd <- as.numeric(sum(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days") - 1, 
                                diff(t$NDZ_sanemsanas_datums[2:3]), diff(t$NDZ_sanemsanas_datums[4:5]),
                                diff(t$NDZ_sanemsanas_datums[6:7]))) 
  } else if (all(t$zk[1:8] == c("21", "11", "50", "51", "50", "51", "50", "51")) && 
             diff(t$NDZ_sanemsanas_datums[4:5]) == 0 &&
             all(sapply(c(1,2,5,6,7), function(i) t$NDZ_sanemsanas_datums[i:(i+1)] != 0))) {
    yt <- y2[v, ]
    yt$dd <- as.numeric(sum(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days"),
                                diff(t$NDZ_sanemsanas_datums[2:3]), diff(t$NDZ_sanemsanas_datums[4:5]),
                                diff(t$NDZ_sanemsanas_datums[6:7]), difftime(t$last_date[8], t$NDZ_sanemsanas_datums[8], units = "days")))
  } else if (all(t$zk[1:8] == c("11", "53", "54", "53", "54", "53", "54", "53")) && 
             all(diff(t$NDZ_sanemsanas_datums[3:4]) == 0) &&
             all(diff(t$NDZ_sanemsanas_datums[7:8]) == 0) && 
             all(diff(t$NDZ_sanemsanas_datums[1:3]) != 0) &&
             all(diff(t$NDZ_sanemsanas_datums[5:7]) != 0)) {
    yt <- y2[v, ]
    yt$dd <- sum(sapply(seq(1,8,by=2), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)])))
  } else if (all(t$zk[1:8] == c("25", "11", "11", "50", "25", "51", "25", "11")) && 
             all(sapply(seq(1, nrow(t), by = 2), function(i) all(diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0))) &&
             t$PS_code[1] == '___________' && t$NM_code[1] == '___________') {
    yt <- y2[v, ]
    yt$dd <- as.numeric(sum(diff(t$NDZ_sanemsanas_datums[1:2]) + 1, 
                                diff(t$NDZ_sanemsanas_datums[7:8]) + 1)) 
  } else if (all(t$zk[c(1,4,6)] == "50") && all(t$zk[c(2,3,5,7)] == "51") && t$zk[8] == "25" && 
              all(diff(t$NDZ_sanemsanas_datums) != 0)) {
    yt <- y2[v, ]
    yt$dd <- as.numeric(sum(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days") - 1, 
                                diff(t$NDZ_sanemsanas_datums[3:4]), diff(t$NDZ_sanemsanas_datums[5:6]),
                                diff(t$NDZ_sanemsanas_datums[7:8]) + 1)) 
  } else if (t$zk[1] == "11" && all(t$zk[c(2,4,6)] == "50") && all(t$zk[c(3,5,7)] == "51") && t$zk[8] == "21" && 
             all(diff(t$NDZ_sanemsanas_datums) != 0)) {
    days <- 0
    for (d in seq(1,7,by=2)) {
      days <- days + as.numeric(difftime(t$NDZ_sanemsanas_datums[d+1], t$NDZ_sanemsanas_datums[d], units = "days"))
    }
    
    yt <- y2[v, ]
    yt$dd <- days + 1
    rm(days, d)
  } else if (all(t$zk[c(1,5,7)] == "50") && all(t$zk[c(2,6,8)] == "51") && t$zk[3] == "53" && t$zk[4] == "54" &&
             all(diff(t$NDZ_sanemsanas_datums) != 0)) {
    days <- as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1
    for (d in seq(2,6,by=2)) {days <- days + as.numeric(difftime(t$NDZ_sanemsanas_datums[d+1], t$NDZ_sanemsanas_datums[d], units = "days"))}
    days <- days + as.numeric(difftime(t$last_date[8], t$NDZ_sanemsanas_datums[8], units = "days")) + 1
    
    yt <- y2[v, ]
    yt$dd <- days
    rm(days, d)
  } else if (all(t$zk[seq(2,8,by=2)] == "50") && all(t$zk[seq(3,8,by=2)] == "51") && t$zk[1] == "11") {
    yt <- y2[v, ] 
    yt$dd <- sum(sapply(seq(1,8,by=2), function(i) as.numeric(difftime(t$NDZ_sanemsanas_datums[i+1], t$NDZ_sanemsanas_datums[i], units = "days"))))
  } else {
    stop("Starpkodi8 iztrūkst apstrādes koda.")
  }
  
  if(is.na(yt$PS_code[1])) {stop("DD NA.")}
  yt$zk <- "combined"
  return(yt)
}

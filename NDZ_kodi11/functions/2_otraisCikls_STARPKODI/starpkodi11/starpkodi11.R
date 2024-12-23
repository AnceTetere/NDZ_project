starpkodi11 <- function(y, t, prev, v) {

  if (all(t$zk[seq(2,11,by=2)] %in% c("50", "91")) && all(t$zk[seq(3,11,by=2)] %in% c("51", "92")) && 
          t$zk[1] %in% c("11","51") && all(diff(t$NDZ_sanemsanas_datums) != 0)) {
            yt <- y[v, ]
            yt$dd <- sum(sapply(seq(1, 10, by=2), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)])),
                             difftime(t$last_date[11], t$NDZ_sanemsanas_datums[11], units = "days")) + 1
  } else if (all(t$zk[c(1,2,6,7)] %in% c("40","91")) && all(t$zk[c(3:5,8:10)] %in% c("41", "92")) && t$zk[11] == "50" && 
             all(sapply(c(1,3,6,8), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) &&
             all(sapply(c(2,4,5,7,9,10), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
    yt <- y[v, ]
    yt$dd <- as.numeric(sum(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days") - 1,
                                diff(t$NDZ_sanemsanas_datums[5:6]), 
                                diff(t$NDZ_sanemsanas_datums[10:11])))
  } else {stop("Starpkodi11 iztrūkst apstrādes koda.")}
  
  if(is.na(yt$PS_code[1])) {stop("Dienas NA.")}
  yt$zk <- "combined"  
  return(yt)
}

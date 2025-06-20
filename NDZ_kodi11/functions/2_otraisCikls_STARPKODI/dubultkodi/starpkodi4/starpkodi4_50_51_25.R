starpkodi4_50_51_25 <- function(y, t, prev, v) {


  yt <- y[v, ]
  
  if (t$zinkod[4] %in% c("41", "51", "54", "92")) {
            if (all(sapply(c(1,3), function(i) all(diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0))) && 
                diff(t$NDZ_sanemsanas_datums[2:3]) != 0) {
              yt$dienas <- 0
            } else if (all(diff(t$NDZ_sanemsanas_datums[1:3]) != 0)) {
              yt$dienas <- as.numeric(sum(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days"), 
                                          diff(t$NDZ_sanemsanas_datums[2:3])))
            } else {stop("starpkodi4_50_51_25: Trūkst izstrādes koda.")}
  } else if (t$zinkod[4] %in% c("40", "50", "53", "91")) {
    if (all(diff(t$NDZ_sanemsanas_datums[1:3]) != 0) && diff(t$NDZ_sanemsanas_datums[3:4]) == 0) {
      yt$dienas <- as.numeric(sum(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days"), 
                                  diff(t$NDZ_sanemsanas_datums[2:3])))
    } else {stop("starpkodi4_50_51_25: Trūkst izstrādes koda.")}
  } else if (t$zinkod[4] %in% c("11", "14", "16", "61")) {
              if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
                if (t$period[1] == "______" && t$PS_code[1] == "___________"   && t$NM_code[1] == "___________" ) {
                 yt$dienas <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1,  
                                  as.numeric(diff(t$NDZ_sanemsanas_datums[2:3])) + 1, 
                                  as.numeric(difftime(t$last_date[4], t$NDZ_sanemsanas_datums[4], units = "days")) + 1) 
              } else {stop("starpkodi4_50_51_25: Trūkst izstrādes koda.")}
        } else {stop("starpkodi4_50_51_25: Trūkst izstrādes koda.")}
  } else {stop("starpkodi4_50_51_25: Trūkst izstrādes koda.")}
    
    
  rm(y, t, prev, v)
  return(yt) 
}

starpkodi5_11_25 <- function(y, t, prev, v){
  
  yt <- y[v,]
  
  if (t$zinkod[3] %in% c("11", "14", "16", "61")) {
    if (t$zinkod[4] %in% c("11", "14", "16", "61")) {
      if(t$zinkod[5] %in% c("40", "50", "53", "91")) {
        if(diff(t$NDZ_sanemsanas_datums[1:2]) == 0 &&all(sapply(2:4, function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) != 0))){
          if(t$PS_code[1] ==  '______________' && t$NM_code[1] ==  '______________') {
            yt$dienas <- sum(as.numeric(diff(t$NDZ_sanemsanas_datums[1:2])), as.numeric(diff(t$NDZ_sanemsanas_datums[4:5])))
          } else {stop("Starpkodi5_11_25: Trūkst izstrādes koda.")}
        } else {stop("Starpkodi5_11_25: Trūkst izstrādes koda.")}
      } else {stop("Starpkodi5_11_25: Trūkst izstrādes koda.")}
    } else if (t$zinkod[4] %in% c("40", "50", "53", "91")) {
             if (t$zinkod[5] %in% c("41", "51", "54", "92")) {
               if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
                 if (t$period[1] == '______' && t$PS_code[1] ==  '______________' && t$NM_code[1] ==  '______________') {
                     yt$dienas <- sum(sapply(c(1,3), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)])), 
                                      as.numeric(difftime(t$last_date[5], t$NDZ_sanemsanas_datums[5], units = "days"))) + 1
          } else {stop("Starpkodi5_11_25: Trūkst izstrādes koda.")}
        } else {stop("Starpkodi5_11_25: Trūkst izstrādes koda.")}
      } else {stop("Starpkodi5_11_25: Trūkst izstrādes koda.")}
    } else {stop("Starpkodi5_11_25: Trūkst izstrādes koda.")}
  } else {stop("Starpkodi5_11_25: Trūkst izstrādes koda.")}  
  
  return(yt)
}

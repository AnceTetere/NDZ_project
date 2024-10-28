starpkodi4_40 <- function(y, t, prev, v){
  
  if (t$zinkod[2] == '41') {
          if (t$zinkod[3] == '41') {
            if (t$zinkod[4] == '50') {
              if (all(sapply(seq(1,4,by=2), function(i) diff(t$NDZ_sanemsanas_datums[i:(i+1)]) == 0)) && diff(t$NDZ_sanemsanas_datums[2:3]) != 0){
                if (t$PS_code[1] == '___________' && t$NM_code[2] == '___________'){
                  yt <- y[v,] 
                  yt$dienas <- as.numeric(sum(diff(t$NDZ_sanemsanas_datums[c(2,1)]), 
                                              diff(t$NDZ_sanemsanas_datums[3:4])))
                } else {stop("Starpkodi4_40 iztrūkst apstrādes koda.")}
              } else {stop("Starpkodi4_40 iztrūkst apstrādes koda.")}
            } else {stop("Starpkodi4_40 iztrūkst apstrādes koda.")}
          } else {stop("Starpkodi4_40 iztrūkst apstrādes koda.")}
  } else if (t$zinkod[2] == '91') {
          if (t$zinkod[3] == '92') {
           if (t$zinkod[4] == '50') {
            if (all(diff(t$NDZ_sanemsanas_datums[2:4]) != 0) && diff(t$NDZ_sanemsanas_datums[1:2]) == 0){
             if (t$PS_code[1] == '________' && t$NM_code[2] == '________'){
            yt <- y[v,] 
            yt$dienas <- as.numeric(sum(difftime(t$NDZ_sanemsanas_datums[2], prev, units = "days") - 1,
                                        diff(t$NDZ_sanemsanas_datums[3:4])))
          } else {stop("Starpkodi4_40 iztrūkst apstrādes koda.")}
        } else {stop("Starpkodi4_40 iztrūkst apstrādes koda.")}
      } else {stop("Starpkodi4_40 iztrūkst apstrādes koda.")}
    } else {stop("Starpkodi4_40 iztrūkst apstrādes koda.")}  
  } else {stop("Starpkodi4_40 iztrūkst apstrādes koda.")}
  
  rm(y, t, prev, v)
  return(yt)
}

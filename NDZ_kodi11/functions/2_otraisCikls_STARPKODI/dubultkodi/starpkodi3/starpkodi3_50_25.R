starpkodi3_50_25 <- function(y, t, prev, v) {
    
  yt <- y[v, ]

  if (t$zinkod[3] %in% c("11", "14", "16", "61")) {
         if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
        #JO PIRMOREIZ
              if ((t$period[1] == '202203' && t$PS_code[1] == '______________' && t$NM_code[1] == '______________') ||
                  (t$period[1] == '202205' && t$PS_code[1] == '______________' && t$NM_code[1] == '______________') ||
                  (t$period[1] == '202205' && t$PS_code[1] == '______________' && t$NM_code[1] == '______________') ||
                  (t$period[1] == '202206' && t$PS_code[1] == '______________' && t$NM_code[1] == '______________')) {
                  yt$dienas <- sum(as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1, 
                                   as.numeric(difftime(t$last_date[3], t$NDZ_sanemsanas_datums[3], units = "days")) + 1)
              } else {stop("starpkodi3_50_25: Iztrūkst apstrādes koda.")}
        } else {stop("starpkodi3_50_25: Iztrūkst apstrādes koda.")}
  } else if (t$zinkod[3] %in% c("41", "51", "54", "92")) {
        if (diff(t$NDZ_sanemsanas_datums[1:2]) != 0 && diff(t$NDZ_sanemsanas_datums[2:3]) == 0) {
          #PĀRBAUDE 10-nieki IZIETA - IESPĒJAMS, KA DATUMU PĀRBAUDE TE IR LIEKA 
                 yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1
        } else if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
                  #IZIETA 10-nieku PĀRBAUDE - IESPĒJAMS KA DATUMU PĀRBAUDE TE IR LIEKA DĒĻ KODU IZKĀRTOJUMA
                    yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1
        } else {stop("starpkodi3_50_25: Iztrūkst apstrādes koda.")}
  } else if (t$zinkod[3] %in% c("40", "50", "53", "91")) {
            if (diff(t$NDZ_sanemsanas_datums[1:2]) != 0 && diff(t$NDZ_sanemsanas_datums[2:3]) == 0) {
                #JO PIRMOREIZ
                if (t$period[1] == '202208' && t$PS_code[1] == '______________' && t$NM_code[1] == '______________') {
                   yt$dienas <- as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1
                } else {stop("starpkodi3_50_25: Iztrūkst apstrādes koda.")}             
            } else {stop("starpkodi3_50_25: Iztrūkst apstrādes koda.")}
  } else {stop("starpkodi3_50_25: Iztrūkst apstrādes koda.")}

  rm(y, t, prev, v)
  return(yt)
}

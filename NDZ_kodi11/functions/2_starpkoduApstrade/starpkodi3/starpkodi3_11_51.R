starpkodi3_11_51 <- function(y, t, prev, v) {
  
  yt <- y[v, ]
  
  if (t$zinkod[3] %in% c("40", "50", "53", "91")) { 
                if (diff(t$NDZ_sanemsanas_datums[1:2]) == 0 && diff(t$NDZ_sanemsanas_datums[2:3]) != 0) {
                    if ((t$PS_code[1] == '______' && t$NM_code[1] == '______') ||
                        (t$period[1] == "______" && t$PS_code[1] == '______' && t$NM_code[1] == '______')) {
                      yt$dd <- as.numeric(diff(t$NDZ_sanemsanas_datums[c(1,3)]))
                    } else {stop("Starpkodi3_11_51: Trūkst izstrādes koda.")}
                } else if (diff(t$NDZ_sanemsanas_datums[1:2]) != 0 && diff(t$NDZ_sanemsanas_datums[2:3]) == 0) {
                yt$dd <- as.numeric(sum(diff(t$NDZ_sanemsanas_datums[c(1,3)]), 
                                            difftime(t$last_date[3], t$NDZ_sanemsanas_datums[2], units = "days") + 1))
              } else if (all(diff(t$NDZ_sanemsanas_datums) == 0)) {
                yt <- y[v, ]
                yt$dd <- as.numeric(difftime(t$last_date[3], t$NDZ_sanemsanas_datums[1], units = "days")) + 1
              } else {stop("Starpkodi3_11_51: Trūkst izstrādes koda.")}
  } else if (t$zinkod[3] %in% c("41", "51", "54", "92")) {
              if (diff(t$NDZ_sanemsanas_datums[2:3]) != 0 && diff(t$NDZ_sanemsanas_datums[1:2]) == 0) {
                if ((t$period[1] == '______' && t$PS_code[1] == '______' && t$NM_code[1] == '______') ||
                    (t$period[1] == '______' && t$PS_code[1] == '______' && t$NM_code[1] == '______')) {
                  yt <- y[v, ]
                  yt$dd <- as.numeric(difftime(t$last_date[3], t$NDZ_sanemsanas_datums[3], units = "days")) + 1
                } else {stop("Starpkodi3_11_51: Trūkst izstrādes koda.")}
              } else {stop("Starpkodi3_11_51: Trūkst izstrādes koda.")} 
  } else {stop("Starpkodi3_11_51: Trūkst izstrādes koda.")}
  
  rm(y, t, prev, v) 
  return(yt) 
}

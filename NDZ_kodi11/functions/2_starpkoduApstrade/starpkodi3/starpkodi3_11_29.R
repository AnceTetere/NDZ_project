starpkodi3_11_29 <- function(y, t, prev, v) {
  
  yt <- y[v,]
  
  if (all(diff(t$NDZ_sanemsanas_datums) == 0)) {
    if (t$zinkod[3] %in% c("40", "50", "53", "91")) {
      if (t$period[1] == "202101" && t$PS_code[1] %in% c('____________', '____________') && 
          t$NM_code[1] %in% c('____________', '____________')) {
        yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[1:2]))
      } else if (t$period[1] == "202101" && 
                 t$PS_code[1] %in% c('____________', '____________', '____________', '____________', '____________', '____________') && 
                 t$NM_code[1] %in% c('____________', '____________', '____________', '____________', '____________', '____________')) {
        yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[c(1,3)]))
      }  else if (t$period[1] == "202101" && t$PS_code[1] == '____________' && t$NM_code[1] == '____________') {
        yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[1:2]))
      } else {stop("Starpkodi3_11: Trūkst izstrādes koda. \n
                   Te ir labi, ka tas bremzējas, jo citādi nesaprast")}
    } else {stop("Starpkodi3_11: Trūkst izstrādes koda.")}
  } else if (diff(t$NDZ_sanemsanas_datums[1:2]) == 0 && diff(t$NDZ_sanemsanas_datums[2:3]) != 0) {
    if (t$zinkod[3] %in% c("40", "50", "53", "91")) {
      if (t$period[1] == "202101" && t$PS_code[1] == '____________' && t$NM_code[1] == '____________') {
        yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[1:2]))
      } else if (t$period[1] == "202101" && t$PS_code[1] == '____________' && t$NM_code[1] == '____________') {
        yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[1:2]))
      } else {stop("Starpkodi3_11: Trūkst izstrādes koda.")}
    } else {stop("Starpkodi3_11: Trūkst izstrādes koda.")}
  } else {stop("Starpkodi3_11: Trūkst izstrādes koda.")}

  rm(y, t, prev, v)
  return(yt)
}

#starpkodi3_11_25 <- function(y2, t, prev, v) {
#  
#  if (t$zinkod[3] == "53" && diff(t$NDZ_sanemsanas_datums[2:3]) == 0 &&
#      diff(t$NDZ_sanemsanas_datums[1:2]) != 0) {
#    yt <- y2[v, ]
#    yt$dienas <- as.numeric(difftime(t$beidz[3], t$sak[1], units = "days"))
#  } else if (t$zinkod[3] == "50" && diff(t$NDZ_sanemsanas_datums[1:2]) != 0 &&
#             diff(t$NDZ_sanemsanas_datums[2:3]) == 0) {
#    yt <- y2[v, ]
#    yt$dienas <- as.numeric(difftime(t$beidz[2], t$sak[1], units = "days"))
#  } else if (t$zinkod[3] == "50" &&  diff(t$NDZ_sanemsanas_datums[1:2]) != 0 &&
#             diff(t$NDZ_sanemsanas_datums[2:3]) == 0) {
#    yt <- y2[v, ]
#    yt$dienas <- as.numeric(difftime(t$beidz[3], t$sak[1], units = "days"))
#  } else if (t$zinkod[3] == "53" && diff(t$NDZ_sanemsanas_datums[1:2]) != 0 &&
#             diff(t$NDZ_sanemsanas_datums[2:3]) == 0) {
#    yt <- y2[v:(v+1), ]
#    yt <- yt[yt$zinkod == "11", ]
#  } else {
#    stop("Starpkodi3_11_25: Trūkst izstrādes koda.")
#  }
#  
#  if(is.na(yt$PS_code[1])) {stop("Dienas NA.")}
#  return(yt) 
#}


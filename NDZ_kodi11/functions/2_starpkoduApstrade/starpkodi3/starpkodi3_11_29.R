starpkodi3_11_29 <- function(y, t, prev, v) {
  
  yt <- y[v,]
  
  if (all(diff(t$NDZ_sanemsanas_datums) == 0)) {
                  if (t$zk[3] %in% c("40", "50", "53", "91")) {
                    if (t$period[1] == "202101" && t$PS_code[1] %in% c('_________', '_________') && 
                        t$NM_code[1] %in% c('_________', '_________')) {
                      yt$dd <- as.numeric(diff(t$NDZ_sanemsanas_datums[1:2]))
                    } else if (t$period[1] == "202101" && 
                               t$PS_code[1] %in% c('_________', '_________', '_________', '_________', '_________', '_________') && 
                               t$NM_code[1] %in% c('_________', '_________', '_________', '_________', '_________', '_________')) {
                      yt$dd <- as.numeric(diff(t$NDZ_sanemsanas_datums[c(1,3)]))
                    }  else if (t$period[1] == "202101" && t$PS_code[1] == '_________' && t$NM_code[1] == '_________') {
                      yt$dd <- as.numeric(diff(t$NDZ_sanemsanas_datums[1:2]))
                    } else {stop("Starpkodi3_11: Trūkst izstrādes koda. \n
                                 Te ir labi, ka tas bremzējas, jo citādi nesaprast")}
                  } else {stop("Starpkodi3_11: Trūkst izstrādes koda.")}
  } else if (diff(t$NDZ_sanemsanas_datums[1:2]) == 0 && diff(t$NDZ_sanemsanas_datums[2:3]) != 0) {
                 if (t$zk[3] %in% c("40", "50", "53", "91")) {
                   if (t$period[1] == "202101" && t$PS_code[1] == '_________' && t$NM_code[1] == '_________') {
                     yt$dd <- as.numeric(diff(t$NDZ_sanemsanas_datums[1:2]))
                   } else if (t$period[1] == "202101" && t$PS_code[1] == '_________' && t$NM_code[1] == '_________') {
                     yt$dd <- as.numeric(diff(t$NDZ_sanemsanas_datums[1:2]))
                   } else {stop("Starpkodi3_11: Trūkst izstrādes koda.")}
                 } else {stop("Starpkodi3_11: Trūkst izstrādes koda.")}
  } else if (diff(t$NDZ_sanemsanas_datums[1:2]) != 0 && diff(t$NDZ_sanemsanas_datums[2:3]) == 0) {
    if (t$zk[3] %in% c("40", "50", "53", "91")) {
      if (t$period[1] == "202201" && t$PS_code[1] == '_________' && t$NM_code[1] == '_________') {
        yt$dd <- as.numeric(diff(t$NDZ_sanemsanas_datums[1:2]))
      } else {stop("Starpkodi3_11: Trūkst izstrādes koda.")}
    } else {stop("Starpkodi3_11: Trūkst izstrādes koda.")}
  } else {stop("Starpkodi3_11: Trūkst izstrādes koda.")}

  rm(y, t, prev, v)
  return(yt)
}

#starpkodi3_11_25 <- function(y2, t, prev, v) {
#  
#  if (t$zk[3] == "53" && diff(t$NDZ_sanemsanas_datums[2:3]) == 0 &&
#      diff(t$NDZ_sanemsanas_datums[1:2]) != 0) {
#    yt <- y2[v, ]
#    yt$dd <- as.numeric(difftime(t$beidz[3], t$sak[1], units = "days"))
#  } else if (t$zk[3] == "50" && diff(t$NDZ_sanemsanas_datums[1:2]) != 0 &&
#             diff(t$NDZ_sanemsanas_datums[2:3]) == 0) {
#    yt <- y2[v, ]
#    yt$dd <- as.numeric(difftime(t$beidz[2], t$sak[1], units = "days"))
#  } else if (t$zk[3] == "50" &&  diff(t$NDZ_sanemsanas_datums[1:2]) != 0 &&
#             diff(t$NDZ_sanemsanas_datums[2:3]) == 0) {
#    yt <- y2[v, ]
#    yt$dd <- as.numeric(difftime(t$beidz[3], t$sak[1], units = "days"))
#  } else if (t$zk[3] == "53" && diff(t$NDZ_sanemsanas_datums[1:2]) != 0 &&
#             diff(t$NDZ_sanemsanas_datums[2:3]) == 0) {
#    yt <- y2[v:(v+1), ]
#    yt <- yt[yt$zk == "11", ]
#  } else {
#    stop("Starpkodi3_11_25: Trūkst izstrādes koda.")
#  }
#  

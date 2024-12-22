starpkodi2_40 <- function(y, t, prev, v) {

  if (t$zk[2] %in% c("21", "22", "23", "24", "25", "29")) {
      yt <- y[v,]
      yt$dd <- as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1 
  } else if (t$zk[2] %in% c("41", "54", "51", "92")) {
      if (t$PS_code[1] == '_________' && t$NM_code[1] == '_________') {
      yt <- y2[v,] 
      yt$dd <- as.numeric(sum(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days") - 1, 
                                  difftime(t$last_date[2], t$NDZ_sanemsanas_datums[2], units = "days") + 1))
      } else {stop("Starpkodi2_40: Trūkst izstrādes koda.")}
  } else {stop("Starpkodi2_40: Trūkst izstrādes koda.")}
  
  rm(y, t, prev, v)
  return(yt)
}

#} else if (t$zk[1] == "91" && t$zk[2] == "21") {
#  yt <- y2[v,] 
#  yt$dd <- as.numeric(difftime(t$beidz[1], prev, units = "days")) - 1 


#  } else if (t$zk[2] == "50" && diff(t$NDZ_sanemsanas_datums[1:2]) != 0 &&
#             t$PS_code[1] == '_________' && t$NM_code[1] == '_________') {
#    yt <- y2[v:(v+1),] 
#    yt <- yt[yt$zk == "53", ]
#  } else {
#    stop("starpkodi2_53: Trūkst izstrādes koda starpkodi2.")
#  }
#  
#  if(is.na(yt$PS_code[1])) {stop("DD NA.")}
#  return(yt)
#} 
#           t$PS_code[1] == '_________' && t$NM_code[1] == '_________') {
#  yt <- y2[v,] 
#  yt$dd <- as.numeric(difftime(t$beidz[1], prev, units = "days")) - 1 

#} else if (t$zk[1] == "40" && t$zk[2] == "51" && t$PS_code[1] == '_________' && t$NM_code[1] == '_________') {
#  days1 <- as.numeric(difftime(t$beidz[1], prev, units = "days")) -1 
#  days2 <- as.numeric(difftime(t$last_date[2], t$sak_darbu[2], units = "days")) + 1 

#  yt <- y2[v,] 
#  yt$dd <- sum(days1, days2)
#  rm(days1,days2)

#starpkodi2_50 <- function(y, t, prev, v) {
  
  #if (t$zk[2] %in% c("21", "22", "23", "24", "25", "29")) {
  #  if (diff(t$NDZ_sanemsanas_datums[1:2]) != 0) {
  #    yt <- y[v,]
  #    yt$dd <- as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1 
  #  } else {stop("Starpkodi2_50: Trūkst izstrādes koda.")}
  #    } else {stop("Starpkodi2_50: Trūkst izstrādes koda.")}
  #  } else {stop("Starpkodi2_50: Trūkst izstrādes koda.")}
  #} else if (t$zk[2] %in% c("50", "53", "40", "91")) {
  #  if (diff(t$NDZ_sanemsanas_datums) != 0) {
  #    yt <- y2[v,]
  #  } else {stop("Starpkodi2_50: Trūkst izstrādes koda.")}
  #} else {stop("Starpkodi2_50: Trūkst izstrādes koda.")}
  #
  
  
  #} else if (t$zk[2] == "11" && diff(t$NDZ_sanemsanas_datums) != 0 &&
  #           t$PS_code[1] == '_________' && t$NM_code[1] == '_________') {
  #  days1 <- as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days"))
  #  days2 <- as.numeric(difftime(t$last_date[2], t$NDZ_sanemsanas_datums[2], units = "days")) + 1
  
  #  yt <- y2[v,] 
  #  yt$dd <- sum(days1, days2)
  #  rm(days1, days2)
  #} else {stop("Starpkodi2_50: Trūkst izstrādes koda.")}
  
#   rm(y, t, prev, v)
#   return(yt)

starpkodi <- function(y2) {
  setwd(paste0(path, "data\\originals\\", year))
  y2 <- y2[order(y2$PS_code, y2$DN_code, y2$NM_code), ]
  
  prev <- as.Date(paste0(substr(y2$period[1], 1, 4), "-", substr(y2$period[1], 5, 6), "-01")) - 1
  z <- data.frame()
  
  for(v in seq(1, nrow(y2), by = 2)) {
    tName <- paste0("NDZ", year, month, "_", y2$zinkod[v])
    load(paste0("starting_", tName, ".RData"))
    t1 <- get(tName)
    t1 <- t1[t1$PS_code == y2$PS_code[v] & t1$NM_code == y2$NM_code[v], ]
    rm(list = tName, tName)
    
    tName <- paste0("NDZ", year, month, "_", y2$zinkod[v+1])
    load(paste0("starting_", tName, ".RData"))
    t2 <- get(tName)
    t2 <- t2[t2$PS_code == y2$PS_code[v+1] & t2$NM_code == y2$NM_code[v+1], ]
    rm(list = tName, tName)
    
    t <- rbind(t1, t2)
    t <- t[order(t$NDZ_sanemsanas_datums), ]
    rownames(t) <- NULL
    rm(t1, t2)
    
    if(!(all(t$PS_code == t$PS_code[1]) && all(t$NM_code == t$NM_code[1]))) {
      stop("ps_CODE nesakritība! Tabula: y2; rinda:", v)
    }
    
    if (nrow(t) == 2) {
      z <- rbind(z, starpkodi2(y2, t, prev))
    } else if(nrow(t) == 3){
      z <- rbind(z, starpkodi3(y2, t, prev))  
    } else if (nrow(t) == 4) {
      z <- rbind(z, starpkodi4(y2, t, prev))
    } else if (nrow(t) == 5) {
      z <- rbind(z, starpkodi5(y2, t, prev))
    } else if (nrow(t) == 6) {
      z <- rbind(z, starpkodi6(y2, t, prev))
    } else if (nrow(t) == 7) {
      z <- rbind(z, starpkodi7(y2, t, prev))
    } else if (nrow(t) == 8) {
      z <- rbind(z, starpkodi8(y2, t, prev))
    } else if (nrow(t) == 9) {
      z <- rbind(z, starpkodi9(y2, t, prev))
    } else if (nrow(t) == 10) {
      z <- rbind(z, starpkodi10(y2, t, prev))
    } else if (nrow(t) == 13) {
      z <- rbind(z, starpkodi13(y2, t, prev))
    } else {
      stop("Trūkst izstrādes koda.")
    }
  }
 
  rm(prev, t, y2)
  return(z) 
  
}

# indivīds te atkārtojas pa reizēm divām.

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
    stop("PS_codes nesakritība! Tabula: y2; rinda:", v)
  }
  
if(nrow(t) == 3) {
  z <- rbind(z, starpkodi3(y2, t, prev))
} else if (nrow(t) == 2) {
  z <- rbind(z, starpkodi2(y2, t))
} else if (nrow(t) == 4) {
  z <- rbind(z, starpkodi4(y2, t, prev))
} else if (nrow(t) == 5) {
  z <- rbind(z, starpkodi5(y2, t, prev))
} else if (nrow(t) == 7) {
  z <- rbind(z, starpkodi7(y2, t, prev))
} else if (nrow(t) == 6) {
  if (t$zinkod[1] == "11" && t$zinkod[2] == "25" && t$NDZ_sanemsanas_datums[1] <= t$NDZ_sanemsanas_datums[2] && t$zinkod[3] == "25" && t$zinkod[4] == "11" && t$zinkod[5] == "81" && t$zinkod[6] == "82" && all(t$NDZ_sanemsanas_datums[4:5] == t$NDZ_sanemsanas_datums[3])) {
    yt <- y2[v:(v+1), ]
    yt <- yt[yt$zinkod == "11", ]
    yt$zinkod <- "combined" #jo starpkodu aprēķins
    
    z <- rbind(z, yt)
    rm(t, yt)
  } else {
    stop("Iztrūkst apstrādes koda.")
  }
} else {stop("Trūkst izstrādes koda.")
  }
}

rm(prev)


setwd(paste0(path, "data\\intermediate_tables\\buildingMonths"))
x <- data.frame()

for (kods in kodu_vektors) {
  tName <- paste0("NDZ", year, month, "_", kods)
  load(paste0("final_", tName, ".RData"))
  y <- get(tName)
  y$zinkod <- kods
  
  x <- rbind(x, y)
  rm(list = tName)
}

rm(y, tName)

#2 Tabulu x pārdala apakštabulās x1 un x2.
x1 <- x %>%
  group_by(PS_code, DN_code, NM_code) %>%
  filter(n() > 1)

x2 <- anti_join(x, x1)
nrow(x) == nrow(x1) + nrow(x2)
rm(x)

if (nrow(unique(x2[, c("period", "PS_code", "DN_code", "NM_code")])) == nrow(x2)) {
  buildingMonth(x2, "1") 
  rm(x2)
} else {
  stop("Apakštabulā x2 visas nav unikālas rindas.")
}

#3 Tabulas x1 apstrāde
x1 <- x1[order(x1$PS_code, x1$DN_code, x1$NM_code), ]

ur <- unique(x1[, c("period", "PS_code", "DN_code", "NM_code")])

sv <- vector()
xb <- data.frame()
xo <- data.frame() #priekš vieniniekiem

i <- 1 # Šis nav testu
while(i <= nrow(ur)) {
  s <- nrow(x1[x1$PS_code == ur$PS_code[i] & x1$DN_code == ur$DN_code[i] & x1$NM_code == ur$NM_code[i], ])
  sv <- append(sv, s)
  if(s > 1) {
    xb <- rbind(xb, x1[x1$PS_code == ur$PS_code[i] & x1$DN_code == ur$DN_code[i] & x1$NM_code == ur$NM_code[i], ])
    } else {
      xo <- rbind(xo, x1[x1$PS_code == ur$PS_code[i] & x1$DN_code == ur$DN_code[i] & x1$NM_code == ur$NM_code[i], ])
      }
  i <- i + 1
}

# Nebija vieninieku, ja nav, varbūt šo lēno daļu izlaid un laid uzreiz dalījumu starp tiem kas 3 un vairāk.
if(nrow(x1) == nrow(xb) + nrow(xo)) {rm(x1, s, i)} 

if (length(sv[sv == 1]) == 0) {rm(xo)}
rm(sv)

#Atlasa tos, kas ir pa divi no tiem, kas ir pa 3.
ur <- unique(xb[, c("period", "PS_code", "DN_code", "NM_code")])

y2 <- data.frame() #priekš diviem
y3 <- data.frame() #priekš trijniekiem

for(i in 1: nrow(ur)){
  s <- nrow(xb[xb$PS_code == ur$PS_code[i] & xb$DN_code == ur$DN_code[i] & xb$NM_code == ur$NM_code[i], ])
  if(s == 2) {
    y2 <- rbind(y2, xb[xb$PS_code == ur$PS_code[i] & xb$DN_code == ur$DN_code[i] & xb$NM_code == ur$NM_code[i], ])
  } else if(s == 3) {
    y3 <- rbind(y3, xb[xb$PS_code == ur$PS_code[i] & xb$DN_code == ur$DN_code[i] & xb$NM_code == ur$NM_code[i], ])
  } else {
    stop("Vienība uzrādas tabulā vairāk par 3 reizēm.")
  }
}

if(nrow(xb) == nrow(y2) + nrow(y3)) {
  rm(xb, s, i, ur)
} else {
  stop("Tabula xb, kas nes unikālās vienības vairāk par vienu, nepārdalījās")
}

#3.2 Tabula xd nes dubultos, kam kodi nesakrīt. 
# Tos sūta starpkodu izstrādei

# Pārbaude
xt <- y2 %>%
  group_by(period, PS_code, DN_code, NM_code, dienas) %>%
  filter(n() > 2)

if(nrow(xt) > 0) {
  stop("Mēneša apstrādes tabulā y2, iztrūkst apstrādes koda!")
} else {
  rm(xt)
}

buildingMonth(starpkodi(y2, 2), "2")
rm(y2)

#3.3 Tabula y3 nes trijniekus, kam kodi nesakrīt. 

# Pārbaude vai tabulā nav vairāk par trijniekiem
xt <- y3 %>%
  group_by(period, PS_code, DN_code, NM_code, dienas) %>%
  filter(n() > 3)

if(nrow(xt) > 0) {
  stop("Mēneša apstrādes tabulā y3, iztrūkst apstrādes koda!")
} else {
  rm(xt)
}

buildingMonth(starpkodi(y3, 3), "3")
rm(y3)

# 4. Tagad sašuj mēneša tabulu kopā
setwd(paste0(path, "data\\intermediate_tables\\buildingMonths"))
x <- data.frame()

for (mt in 1:3) {
  tName <- paste0("month", mt, "_", year, month)
  load(paste0(tName, ".RData"))
  y <- get(tName)
  
  x <- rbind(x, y)
  rm(list = tName)
  # TODO: te ieliec, ka procesā, izmantojot tName, tabulu izdzēš no mapes buildingMonths.
}

x <- x[ , c("period", "PS_code", "DN_code", "NM_code", "dienas")]
  
rm(y, tName, mt)

# Pārbauda gala tabulu.
x <- x[order(x$PS_code, x$DN_code, x$NM_code), ]

if(sum(duplicated(x[c("PS_code", "DN_code", "NM_code")])) > 0) {
  stop("Mēneša gala tabulā uzrādās dubultnieki")
} else {
  cat("PĀRBAUDE IZIETA: Gala tabulā dubultnieku nav.\n")
}

#2. Pārbauda, vai kādam dienu skaits nav vairāk nekā mēnesī dienu.
if (nrow(x[x$dienas > 31, ]) == 0) { #TODO: Te priekš tā 31, vajag iztrādāt funkciju, kur, vadoties pēc perioda ailes, tiek pānemts datums salīdzināšanai.
  cat("PĀRBAUDE IZIETA: Kodu gala tabulā nevienam nav 
      vairāk dienu par dienu skaitu mēnesī.\n")
} else {
  stop(cat("STOP: Kad izstrādā šo, tad noņem STOP. 
           KODU GALA TABULĀ UZ UNIKĀLO INDIVĪDU SASUMMĒTĀS DIENAS 
           PĀRSNIEDZ DIENU SKAITU MĒNESĪ.\n"))
}

if (nrow(x[x$dienas < 0, ]) == 0) { #TODO: Te priekš tā 31, vajag iztrādāt funkciju, kur, vadoties pēc perioda ailes, tiek pānemts datums salīdzināšanai.
  cat("PĀRBAUDE IZIETA: Kodu gala tabulā nevienam nav 
      mazāk dienu par 0.\n")
} else {
  stop(cat("STOP: Kad izstrādā šo, tad noņem STOP. 
           KODU GALA TABULĀ UZ UNIKĀLO INDIVĪDU SASUMMĒTĀS DIENAS 
           IR MAZĀK PAR NULLI.\n"))
}

#3. Ieliec nosaukumu un noglabā mēneša mapē.
mtab_nos <- paste0("NDZ", year, month, "_dienas")
assign(mtab_nos, x)

if(!dir.exists(paste0(path, "data\\output\\", year))) {
  dir.create(paste0(path, "data\\output\\", year))
}

setwd(paste0(path, "data\\output\\", year))
save(list = mtab_nos, file = paste0("final_", mtab_nos, ".RData"))
write.table(x, file = paste0(mtab_nos, ".csv"), sep = ";", col.names = TRUE, qmethod = "double", row.names = FALSE)

#4. Nokop aiz sevis. TODO IZSTRĀDĀ
setwd(paste0(path, "data\\intermediate_tables\\buildingMonths"))
for(kods in kodu_vektors){
  file.remove(paste0("final_NDZ", year, month, "_", kods, ".RData"))
}
rm(list = mtab_nos, mtab_nos, x, kodu_vektors, kods, month, path, year)

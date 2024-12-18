x <- data.frame()

for (kods in kodu_vektors) {
  tName <- paste0("NDZ", year, month, "_", kods)
  load(paste0("data/starptabulas/buildingMM/final_", tName, ".RData"))
  y <- get(tName)
  y$zinkod <- kods
  
  x <- rbind(x, y)
  rm(list = tName)
}

rm(y, tName)

#2 Tabulu x pārdala apakštabulās x1 un x2.

x1 <- x %>%
  group_by(PS_code, DN_code, NM_code) %>%
  dplyr::filter(n() > 1)

x2 <- anti_join(x, x1)
if (nrow(x) == nrow(x1) + nrow(x2)) {rm(x)} else {stop("MM: Rindas neatbilst!")}

if (nrow(distinct(x2, period, PS_code, DN_code, NM_code)) == nrow(x2)) {
  buildingMonth(x2, "1", md) 
  rm(x2)
} else {stop("Apakštabulā x2 visas nav unikālas rindas.")}

#3 Tabulas x1 apstrāde
x1 <- arrange(x1, PS_code, DN_code, NM_code)

#############################################____________________NOŅEMAMĀ DAĻA_______________________
#Te tā tabula atnāk kaut kāda netīra, un tajā vēl aizvien var būt vieninieki un vairāk par divi.
#ur <- distinct(x1, period, PS_code, DN_code, NM_code)

#sv <- vector()
#xb <- data.frame()
#xo <- data.frame() #priekš vieniniekiem

#while(i <= nrow(ur)) {
  #s <- nrow(x1[x1$PS_code == ur$PS_code[i] & x1$DN_code == ur$DN_code[i] & x1$NM_code == ur$NM_code[i], ])
#  t <- filter(x1, PS_code == ur$PS_code[i] & DN_code == ur$DN_code[i] & NM_code == ur$NM_code[i])
  #sv <- append(sv, s)
#  sv <- append(sv, nrow(t))
#  if(nrow(t) > 1) {xb <- rbind(xb, t)
#  } else {xo <- rbind(xo, t)}
#  i <- i + 1}

# Nebija vieninieku, ja nav, varbūt šo lēno daļu izlaid un laid uzreiz dalījumu starp tiem kas 3 un vairāk.
#if(nrow(x1) == nrow(xb) + nrow(xo)) {rm(x1, t, i)} 

#if (length(sv[sv == 1]) == 0) {rm(xo)}
#x1 <- xb
#rm(sv)
#############################################____________________NOŅEMAMĀS DAĻAS BEIGAS_______________________

#Atlasa tos, kas ir pa divi no tiem, kas ir pa 3.
ur <- distinct(x1, period, PS_code, DN_code, NM_code)

y2 <- data.frame() #priekš diviem
y3 <- data.frame() #priekš trijniekiem

#!Šī daļa ir lēna.
#Izdomā vēlāk kā šo optimatizēt vai izvilkt kā funkciju.
for(i in 1:nrow(ur)){
  t <- dplyr::filter(x1, PS_code == ur$PS_code[i] & DN_code == ur$DN_code[i] & NM_code == ur$NM_code[i])
  s <- nrow(t)
  if(s == 2) {
    y2 <- rbind(y2, t)
  } else if(s == 3) {
    y3 <- rbind(y3, t)
  } else {
    stop("Vienība uzrādas tabulā vairāk par 3 reizēm.")
  }
}

if(nrow(x1) == nrow(y2) + nrow(y3)) {rm(t, i, ur, x1)
} else {stop("Tabula x1, kas nes vienības vairāk par vienu, nepārdalījās")}

# Pārbaude
xt <- y2 %>%
  group_by(period, PS_code, DN_code, NM_code, dienas) %>%
  dplyr::filter(n() > 2)

if(nrow(xt) > 0) {
  stop()
} else {rm(xt)}

buildingMonth(starpkodi(y2, 2), "2", md)
rm(y2)

#3.3 Tabula y3 nes trijniekus, kam kodi nesakrīt. 
# Pārbaude 
xt <- y3 %>%
  group_by(period, PS_code, DN_code, NM_code, dienas) %>%
  filter(n() > 3)

if(nrow(xt) > 0) {
  stop()
} else {
  rm(xt)
}

buildingMonth(starpkodi(y3, 3), "3", md)
rm(y3)

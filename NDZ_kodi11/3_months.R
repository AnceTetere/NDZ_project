#1 Sašuj visas kodu tabulas uz mēnesi, pievienojot aili zinkod, 
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
#Apakštabula x1 nošķir tos, kuri tabulā x uzrādās vairākkārt.

x1 <- x %>%
  group_by(PS_code, DN_code, NM_code) %>%
  filter(n() > 1)

x2 <- anti_join(x, x1)
nrow(x) == nrow(x1) + nrow(x2)
rm(x)

# Pārbauda vai x2 neviena diena nepārsniedz dienu skaitu mēnesī 
# šo definējumu vajag izstrādāt.
if (nrow(unique(x2[, c("period", "PS_code", "DN_code", "NM_code")])) == nrow(x2)) {
  buildingMonth(x2, "1") 
  rm(x2)
} else {
  stop("Apakštabulā x2 visas nav unikālas rindas.")
}

#3 Tabulas x1 apstrāde
x1 <- x1[order(x1$PS_code, x1$DN_code, x1$NM_code), ]

#Te tā tabula atnāk kaut kāda netīra, un tajā vēl aizvien var būt vieninieki un vairāk par divi.
#3.1 No tabulas x1 izdala indivīdus, kas uzrādās veinreiz, un kas vairāk.
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

# Tabula xd nes vienības pa divi.
# Sākumā es domāju, ka jānošķir tabulā y1 tās, kuras ir identiskas un savienot tās, 
# taču tā nevar. Identisks bija tas, kur kodi atšķiras, bet dienu sarēķins vienāds.
# Tabula y2 nesīs divniekus, kas nav identiski, taču tā nevar.
# Tas darbojās tad, kad bija pārklājums ar 81 un 82, kodiem, taču tagad, ka tie ir izņemti
# tā pieeja ir kļūdaina. Tā nevajag darīt.

#Nez, te tomēr tik droši nevar - šos tomēr laid caur lielo apstrādi bez šī collapse
#xd <- xd[order(xd$PS_code, xd$DN_code, xd$NM_code), ]
#ur <- unique(xd[, c("period", "PS_code", "DN_code", "NM_code")])

#y1 <- data.frame() # identiskie divnieki
#y2 <- data.frame() # neindentiskie divnieki

#for(i in 1:nrow(ur)){
#  s <- xd[xd$PS_code == ur$PS_code[i] & xd$DN_code == ur$DN_code[i] & xd$NM_code == ur$NM_code[i], ]
#  if(s$PS_code[1] == s$PS_code[2] && s$DN_code[1] == s$DN_code[2] && s$NM_code[1] == s$NM_code[2] && s$dienas[1] == s$dienas[2]) {
#    y1 <- rbind(y1, s[1, ])
#    y1$zinkod <- "collapsed"
#  } else {
#    y2 <- rbind(y2, s)
#  }
#}

#if(nrow(xd) == nrow(y1)*2 + nrow(y2)) {
#  rm(xd, s, i, ur)
#} else {
#  stop("Tabula xd, kas nes dubultās vienības, nepārdalījās")
#}

#buildingMonth(y1, "2") #### NOŅEMU DIVNIEKU ŠIM UN PADODU TO UZ PRIEKŠU. PAGAIDĀM VISS IR CIETI.
#rm(y1)

#3.2 Tabula xd nes dubultos, kam kodi nesakrīt. 
# Tos sūta starpkodu izstrādei

# Pārbaude
xt <- y2 %>%
  group_by(period, PS_code, DN_code, NM_code, dienas) %>%
  filter(n() > 2)

if(nrow(xt) > 0) {
  stop("Mēneša apstrādes tabulā y2, kas nes indivīdus, kuru dienas skaitās ar dažādiem kodiem,
       daži uzrādās vairāk par 2 reizēm. Iztrūkst apstrādes koda!")
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
  stop("Mēneša apstrādes tabulā y3, kas nes indivīdus, kuru dienas skaitās ar dažādiem kodiem,
       daži uzrādās vairāk par 3 reizēm. Iztrūkst apstrādes koda!")
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
  # 75729
  # TODO: te ieliec, ka procesā, izmantojot tName, tabulu izdzēš no mapes buildingMonths.
}

x <- x[ , c("period", "PS_code", "DN_code", "NM_code", "dienas")]
  
rm(y, tName, mt)

# Pārbauda gala tabulu.
x <- x[order(x$PS_code, x$DN_code, x$NM_code), ]

if(sum(duplicated(x[c("PS_code", "DN_code", "NM_code")])) > 0) {
  stop("Mēneša gala tabulā uzrādās dubultnieki")
} else {
  cat("Gala tabulā dubultnieku nav.\n")
}

#2. Pārbauda, vai kādam dienu skaits 
if (nrow(x[x$dienas > 31, ]) == 0) { #TODO: Te priekš tā 31, vajag iztrādāt funkciju, kur, vadoties pēc perioda ailes, tiek pānemts datums salīdzināšanai.
  cat("PĀRBAUDE IZIETA: Kodu gala tabulā nevienam nav 
      vairāk dienu par dienu skaitu mēnesī.\n")
} else {
  stop(cat("STOP: Kad izstrādā šo, tad noņem STOP. 
           KODU GALA TABULĀ UZ UNIKĀLO INDIVĪDU SASUMMĒTĀS DIENAS 
           PĀRSNIEDZ DIENU SKAITU MĒNESĪ.\n"))
}

#3. Ieliec nosaukumu un noglabā mēneša mapē.
mtab_nos <- paste0("NDZ", year, month, "_dienas")
assign(mtab_nos, x)

setwd(paste0(path, "data\\intermediate_tables\\buildingMonths"))
save(list = mtab_nos, file = paste0("final_", mtab_nos, ".RData"))

setwd(paste0(path, "data\\output"))
write.table(x, file = paste0(mtab_nos, ".csv"), sep = ";", col.names = TRUE, qmethod = "double", row.names = FALSE)

rm(list = mtab_nos, mtab_nos, x, kodu_vektors, kods)

#4. Nokop aiz sevis. TODO IZSTRĀDĀ
#setwd(paste0(path, "data\\intermediate_tables\\buildingMonths"))
#file.remove("tabs_NDZ.rds", "temp_rows.RDS", "temp_NDZ.RData")  #1 Sašuj visas kodu tabulas uz mēnesi, pievienojot aili zinkod, kas norāda, no kura kodu kopsavilkuma tas nāk. 
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
#Apakštabula x1 nošķir tos, kuri tabulā x uzrādās vairākkārt.

x1 <- x %>%
  group_by(PS_code, DN_code, NM_code) %>%
  filter(n() > 1)

x2 <- anti_join(x, x1)
nrow(x) == nrow(x1) + nrow(x2)
rm(x)

# Pārbauda vai x2 neviena diena nepārsniedz dienu skaitu mēnesī 
# Te tiek vērtēts oktobris, bet šo definējumu vajag izstrādāt.
if (nrow(unique(x2[, c("period", "PS_code", "DN_code", "NM_code")])) == nrow(x2)) {
  buildingMonth(x2, "1") 
  rm(x2)
} else {
  stop("Apakštabulā x2 visas nav unikālas rindas.")
}

#3 Tabulas x1 apstrāde
x1 <- x1[order(x1$PS_code, x1$DN_code, x1$NM_code), ]

# Pirmkārt nošķirt tos, kuriem dienu sarēķins ar diviem dažādiem kodiem ir identisks.
#3.1 No tabulas x1 izdala šos indivīdus 
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

if(nrow(x1) == nrow(xb) + nrow(xo)) {
  rm(x1, s, i)
} else if (length(sv[sv == 1]) == 0) {
  rm(xo)
}

#Atlasa tos, kas ir pa divi no tiem, kas ir pa 3.
ur <- unique(xb[, c("period", "PS_code", "DN_code", "NM_code")])

xd <- data.frame()
y3 <- data.frame() #priekš vieniniekiem

for(i in 1: nrow(ur)){
  s <- nrow(xb[xb$PS_code == ur$PS_code[i] & xb$DN_code == ur$DN_code[i] & xb$NM_code == ur$NM_code[i], ])
  if(s == 2) {
    xd <- rbind(xd, xb[xb$PS_code == ur$PS_code[i] & xb$DN_code == ur$DN_code[i] & xb$NM_code == ur$NM_code[i], ])
  } else if(s == 3) {
    y3 <- rbind(y3, xb[xb$PS_code == ur$PS_code[i] & xb$DN_code == ur$DN_code[i] & xb$NM_code == ur$NM_code[i], ])
  } else {
    stop("Vienība uzrādas tabulā vairāk par 3 reizēm.")
  }
}

if(nrow(xb) == nrow(xd) + nrow(y3)) {
  rm(xb, s, i)
} else {
  stop("Tabula xb, kas nes unikālās vienības vairāk par vienu, nepārdalījās")
}

# Tabula xd nes vienības pa divi.
# Tagad jānošķir tabulā y1 tās, kas ir identiskas un savienot tās.
# Tabula y2 nesīs divniekus, kas nav identiki.

xd <- xd[order(xd$PS_code, xd$DN_code, xd$NM_code), ]
ur <- unique(xd[, c("period", "PS_code", "DN_code", "NM_code")])

y1 <- data.frame()
y2 <- data.frame() #priekš vieniniekiem

for(i in 1:nrow(ur)){
  s <- xd[xd$PS_code == ur$PS_code[i] & xd$DN_code == ur$DN_code[i] & xd$NM_code == ur$NM_code[i], ]
  if(s$PS_code[1] == s$PS_code[2] && s$DN_code[1] == s$DN_code[2] && s$NM_code[1] == s$NM_code[2] && s$dienas[1] == s$dienas[2]) {
    y1 <- rbind(y1, s[1, ])
    y1$zinkod <- "collapsed"
  } else {
    y2 <- rbind(y2, s)
  }
}

if(nrow(xd) == nrow(y1)*2 + nrow(y2)) {
  rm(xd, s, i, ur)
} else {
  stop("Tabula xd, kas nes dubultās vienības, nepārdalījās")
}

buildingMonth(y1, "2")
rm(y1)

#3.2 Tabula y2 nes dubultos, kam kodi nesakrīt. 
# Tos sūta starpkodu izstrādei

# Pārbaude
xt <- y2 %>%
  group_by(period, PS_code, DN_code, NM_code, dienas) %>%
  filter(n() > 2)

if(nrow(xt) > 0) {
  stop("Mēneša apstrādes tabulā y2, kas nes indivīdus, kuru dienas skaitās ar dažādiem kodiem,
       daži uzrādās vairāk par 2 reizēm. Iztrūkst apstrādes koda!")
} else {
  rm(xt)
}

# Tagad katrs jāizstrādā atsevišķi, kas tur notiek

#šito pārstrādā divās apakštabulās - nezinu kā tas tika cauri sietam



#############



x <- x[order(x$PS_code, x$DN_code, x$NM_code), ]

cat("Pārbauda dubultos indivīdus, definētus kā PS_code == PS_code,
    DN_code == DN_code, NM_code == NM_code. Ja ir, tad sasummē to dienas.\n")

y <- x[duplicated(x[c("PS_code", "DN_code", "NM_code")]), ]
y[order(y$PS_code), ]
z <- x[x$PS_code %in% y$PS_code, ]




if(sum(duplicated(x[c("PS_code", "DN_code", "NM_code")])) > 0) {
  x <- x %>%
    group_by(period, PS_code, DN_code, NM_code) %>%
    summarise(
      dienas = sum(dienas, na.rm = TRUE)
    ) %>%
    arrange(PS_code)
} else {
  cat("Gala tabulā dubultnieku nav.\n")
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

x <- data.frame()

for (kods in kodu_vektors) {
  tName <- paste0("NDZ", year, month, "_", kods)
  load(paste0("data/starptabulas/buildingMonths/final_", tName, ".RData"))
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
  dplyr::filter(n() > 1)

x2 <- anti_join(x, x1)
if (nrow(x) == nrow(x1) + nrow(x2)) {rm(x)} else {stop("Months: Rindas neatbilst!")}

# Pārbauda vai x2 nevienam dienas nepārsniedz dienu skaitam mēnesī 
md <- menesa_dienu_skaits(year, month)
if (nrow(distinct(x2, period, PS_code, DN_code, NM_code)) == nrow(x2)) {
  buildingMonth(x2, "1", md) #x2: tabula; "1": mēnēša daļa; md: dienu skaits mēnesī, kas nāk no funkcijas menesa_dienu_skaits(x) 
  rm(x2)
} else {stop("Apakštabulā x2 visas nav unikālas rindas.")}

#3 Tabulas x1 apstrāde
x1 <- arrange(x1, PS_code, DN_code, NM_code)

#############################################____________________NOŅEMAMĀ DAĻA_______________________
#Te tā tabula atnāk kaut kāda netīra, un tajā vēl aizvien var būt vieninieki un vairāk par divi.
#3.1 No tabulas x1 izdala indivīdus, kas uzrādās veinreiz, un kas vairāk.
#Ļoti lēns LOOPs, GAIDI.
#ur <- distinct(x1, period, PS_code, DN_code, NM_code)

#sv <- vector()
#xb <- data.frame()
#xo <- data.frame() #priekš vieniniekiem

#i <- 1 # Šis nav testu
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
#############################################____________________NOŅEMAMĀ DAĻA_______________________

#Atlasa tos, kas ir pa divi no tiem, kas ir pa 3.
ur <- distinct(x1, period, PS_code, DN_code, NM_code)

y2 <- data.frame() #priekš diviem
y3 <- data.frame() #priekš trijniekiem

#!Šī daļa ir lēna.
#Izdomā vēlāk kā šo optimatizēt vai izvilkt kā funkciju.
#for(i in 1:nrow(ur)){
#  t <- dplyr::filter(x1, PS_code == ur$PS_code[i] & DN_code == ur$DN_code[i] & NM_code == ur$NM_code[i])
#  s <- nrow(t)
#  if(s == 2) {
#    y2 <- rbind(y2, t)
#  } else if(s == 3) {
#    y3 <- rbind(y3, t)
#  } else {
#    stop("Vienība uzrādas tabulā vairāk par 3 reizēm.")
#  }
#}

# Savieno ur ar x1, lai izfiltrētu rindas, kas atbilst nosacījumiem (2 vai 3 rindas)
result <- x1 %>%
  inner_join(ur, by = c("PS_code", "DN_code", "NM_code")) %>%
  group_by(PS_code, DN_code, NM_code) %>%
  mutate(row_count = n()) %>%  # Count rows per group
  ungroup()

# Nodala apakštabulas y2 un y3 atbilstoši rindu skaitam uz unikālo indivīdu.
y2 <- result %>% dplyr::filter(row_count == 2) %>% select(-row_count)
y3 <- result %>% dplyr::filter(row_count == 3) %>% select(-row_count)

# Check for invalid groups (more than 3 rows)
if (any(result$row_count > 3)) {
  stop("Vienība uzrādas tabulā vairāk par 3 reizēm.")
}


if(nrow(x1) == nrow(y2) + nrow(y3)) {rm(t, i, ur, x1)
} else {stop("Tabula x1, kas nes vienības vairāk par vienu, nepārdalījās")}


# Pārbaude
xt <- y2 %>%
  group_by(period, PS_code, DN_code, NM_code, dienas) %>%
  dplyr::filter(n() > 2)

if(nrow(xt) > 0) {
  stop("Mēneša apstrādes tabulā y2, kas nes indivīdus, kuru dienas skaitās ar dažādiem kodiem,
       daži uzrādās vairāk par 2 reizēm. Iztrūkst apstrādes koda!")
} else {rm(xt)}

buildingMonth(starpkodi(y2, 2), "2", md)
rm(y2)

#3.3 Tabula y3 nes trijniekus, kam kodi nesakrīt. 

# Pārbaude vai tabulā nav vairāk par trijniekiem
xt <- y3 %>%
  group_by(period, PS_code, DN_code, NM_code, dienas) %>%
  dplyr::filter(n() > 3)

if(nrow(xt) > 0) {
  stop("Mēneša apstrādes tabulā y3, kas nes indivīdus, kuru dienas skaitās ar dažādiem kodiem,
       daži uzrādās vairāk par 3 reizēm. Iztrūkst apstrādes koda!")
} else {
  rm(xt)
}

buildingMonth(starpkodi(y3, 3), "3", md)
rm(y3)


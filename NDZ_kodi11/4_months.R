----
#1 Sašuj visas kodu tabulas uz mēnesi, pievienojot aili zinkod, 
#kas norāda, no kura kodu kopsavilkuma tas nāk. 
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

# Pārbauda vai x2 neviena diena nepārsniedz dienu skaitu mēnesī 
md <- menesa_dienu_skaits(year, month)
if (nrow(distinct(x2, period, PS_code, DN_code, NM_code)) == nrow(x2)) {
  buildingMonth(x2, "1", md) #x2: tabula; "1": mēnēša daļa; md: dienu skaits mēnesī, kas nāk no funkcijas menesa_dienu_skaits(x) 
  rm(x2)
} else {stop("Apakštabulā x2 visas nav unikālas rindas.")}

#3 Tabulas x1 apstrāde
#Definē unikālos indivīdus tabulā x1.
ur <- x1 %>% arrange(x1, PS_code, DN_code, NM_code) %>% 
  distinct(period, PS_code, DN_code, NM_code)

#Atlasa tos, kas ir pa divi, no tiem, kas ir pa 3.
#Savieno ur ar x1, lai noteiktu rindu skaitu uz indivīdu.
rs <- x1 %>%
  inner_join(ur, by = c("PS_code", "DN_code", "NM_code")) %>%
  group_by(PS_code, DN_code, NM_code) %>%
  mutate(row_count = n()) %>%  #Saskaita rindu skaitu uz unikālo indivīdu
  ungroup()

# Nodala apakštabulas y2 un y3 atbilstoši rindu skaitam uz unikālo indivīdu.
if(!any(rs$row_count > 3)) {
  y2 <- rs %>% dplyr::filter(row_count == 2) %>% mutate(period = period.x) %>% 
    select(period, PS_code, DN_code, NM_code, dienas, zinkod)
  y3 <- rs %>% dplyr::filter(row_count == 3) %>% mutate(period = period.x) %>% 
    select(period, PS_code, DN_code, NM_code, dienas, zinkod)
  if(nrow(x1) == nrow(y2) + nrow(y3)) {rm(ur, x1, rs)
  } else {stop("Tabula x1, kas nes vienības vairāk par vienu, nepārdalījās")}
} else {stop("4_months: Tabulā x1 ir indivīdi, kas uzrādās ar vairāk kā 3 kodiem.")}


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

if (nrow(y3) > 0) {
buildingMonth(starpkodi(y3, 3), "3", md)
rm(y3)}

processingFours_1121 <- function(a, o, kods) {
  a4_1 <- data.frame(); a4_2 <- data.frame(); a4_3 <- data.frame()
  #a <- x4
  
  if (diff(a$NDZ_sanemsanas_datums[2:3]) == 0 && 
      all(sapply(c(1,3), function(i) diff(a$NDZ_sanemsanas_datums[i:(i+1)]) != 0))) {
              #Par cik pirmā reize, nezinu vai šo var vispārināt.
              #G1: Dziļāka izpēte liek domāt par ievades kļūdu, ka kods "51" 2022-02-16 ir lieks.
              #G2: Indivīds uzsāk darbu, bet neuzsāk, uzsāk vēlāk un tanī pat dienā tiek atlaists. 
              #    Uzsāk ar pēdējo sākšanas kodu.
              #G3: Gadījums PS_code = 'PK416A46EF6'; NM_code = '50003505311' atklājas tikai pēc dziļākas izpētes,
              #    un vēl tur ir kļūda, kas jālabo caur ADJ50, bet varētu būt savādāk. Nezinu, vai šo var
              #    vispārināt, bet pagaidām visi sekojošie pakārtojas sekojošajam dalījumam.
          if ((a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') ||
              (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') ||
              (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') ||
              (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') ||
              (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') ||
              (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') ||
              (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') ||
              (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') ||
              (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________')) {
                   #Gadījumā a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________' 
                   #pēdējais zinkod == '92' 2022-05-08 ir lieks, vai kļūda - neizskaidrojams.
                     a4_2 <- a[2:3, ]; a4_1 <- a[4, ]
                     if (kods %in% c("40", "50", "53") && o == "4") {ZERO_minus(a %>% slice(1))}
          } else if (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') {
                     #Padomā, kā šos nošķirt, jo šajā gadījumā 2-rā un 3-šā rindas ir apmetušās riņķī.
                     #Te, piemēram, tas attiecas uz kodu '11', bet neesmu pārliecunāta vai šis vispārinās.
                     #JO PIRMOREIZ
                     a4_2 <- a[1:2, ]; a4_1 <- a[4, ]
                     if (kods %in% c("40", "50", "53") && o == "4") {ZERO_minus(a %>% slice(1))}
          } else {stop("processingFours_1121 trūkst apstrādes koda.")}
  } else if (all(diff(a$NDZ_sanemsanas_datums) != 0)) {
           #IZGĀJIS 10-nieku PĀRBAUDI
                   a4_2 <- a[2:3, ]; a4_1 <- a[4, ]
                   if (kods %in% c("40", "50", "53") && o == "4") {ZERO_minus(a %>% slice(2))}
  } else if (all(diff(a$NDZ_sanemsanas_datums[1:3]) == 0) && diff(a$NDZ_sanemsanas_datums[3:4]) != 0) {
    #Indivīds ar diviem kodiem atgriežas no BK atvaļinājuma un aiziet nākamajā un atgriežas.
    #Par cik pirmā reize, nezinu vai šo var vispārināt.
    #CITĀDI VISS IR KĀRTĪBĀ
    if (a$period[1] == '______' && a$PS_code[1] ==  '______________' && a$NM_code[1] ==  '______________') {
      a4_2 <- a[2:3, ]; a4_1 <- a[4, ]
      if (kods %in% c("40", "50", "53") && o == "4") {ZERO_minus(a %>% slice(1:2))}
    } else {stop("processingFours_1121 trūkst apstrādes koda.")}
  } else {stop("processingFours_1121 trūkst apstrādes koda.")}
  
  return(list(x4_uzVieniniekiem = a4_1,
              x4_trueDoubles = a4_2,
              x4_uzTrijniekiem= a4_3))
}

processingOnes <- function(x, o) {
    if (as.numeric(o) > 1) { #šo neņem ārā, šis ir svarīgi, jo tad, kad tas nāk no tabulas NDZ_1, tur visi ir unikālie, un tā tabula ir milzīga, un uzkarina visu programmu dalot to, kad tur nav nekā ko dalīt.

    x1 <- x
    x1 <- x1[order(x1$PS_code, x1$DN_code, x1$NM_code, x1$NDZ_sanemsanas_datums, x1$sak_beidz), ]

    if (nrow(x1) > 0) {
      x_uzVieniniekiem <- data.frame()
      
      #for (p in 1:floor((as.numeric(o) / 2))) {
        x2 <- data.frame()
        x3 <- data.frame()
        
        
        if (length(unique(x1$PS_code)) == nrow(x1)) {
          x_uzVieniniekiem <- x1
        } else {
          r <- 1  # ! šo nedzēs, šis nav no testēšanas palicis r, bet ir nepieciešams while loopam.
          while(r <= nrow(x1)) {
            if (ifelse(is.na(doublesTest(r, x1)), FALSE, doublesTest(r, x1))) {
              if (x1$zinkod[r] == x1$zinkod[r + 1]) {
                x2 <- rbind(x2, x1[c(r, r + 1), ])
                r <- r + 2
              } else if (ifelse(is.na(x1$beidz_darbu[r] < x1$sak_darbu[r + 1]), FALSE, (x1$beidz_darbu[r] < x1$sak_darbu[r + 1])))  {
                x_uzVieniniekiem <- rbind(x_uzVieniniekiem, x1[c(r, r + 1), ])
                r <- r + 2
              } else {
                x3 <- rbind(x3, x1[c(r, r + 1), ])
                r <- r + 2
              }
            } else {
              x_uzVieniniekiem <- rbind(x_uzVieniniekiem, x1[r, ])
              r <- r+1
            }
          }
          
          
          if (nrow(x1) == nrow(x2) + nrow(x3) + nrow(x_uzVieniniekiem)) {
            rm(r, x1)
          } else {
            stop(cat(
              "ERROR: Atvasināto tabulu x2 un x3 nesakrīt ar mātes tabulu x1."
            ))
          }
          
          if (nrow(x2) > 0) {
            x_uzVieniniekiem <-
              rbind(x_uzVieniniekiem,
                    F_doubleStartEnd_codesMatch(x2))
          }
          
          if (nrow(x3) > 0) {
            x_uzVieniniekiem <-
              rbind(x_uzVieniniekiem,
                    F_doubleStartEnd_codesDiffer(x3))}
          rm(x2, x3)
        }
      }
      
      x <- x_uzVieniniekiem
      rm(x_uzVieniniekiem)
  }

  #1 Sakārto tabulu
  x <- x[order(x$PS_code, x$DN_code, x$NM_code, x$NDZ_sanemsanas_datums, x$sak_beidz), ]
  rownames(x) <- NULL
  
  #2 Aprēķini dienas no darba sākšanas datuma līdz mēneša pēdējai dienai, un ievieto tās ailē [dienas].
  #  Pieskaita 1, jo gan mēneša pēdējai datums, gan darba sākšanas diena piederās dienu skaitam.
  x$dienas[x$sak_beidz == "1"] <- as.numeric(difftime(x$last_date[x$sak_beidz == "1"], x$NDZ_sanemsanas_datums[x$sak_beidz == "1"], units = "days")) + 1
  
  #3 Aprēķini dienas no iepriekšējā mēneša pēdējās dienas līdz beidz_darbu, un ievieto tās ailē [dienas]
  #3.1 Izrēķini iepriekšējā mēneša pēdējo dienu
  prev <- as.Date(format(x$last_date[1], "%Y-%m-01")) - 1
  
  #3.2 Aprēķina nostrādātās dienas pret iepriekšējā mēneša pēdējo dienu.
  #    Dienas rēķina no iepriekšējā mēneša beigu datuma līdz darba beigšanas datumam (x$NDZ_sanemsanas_datums[x$sak_beidz == "2"])
  x$dienas[x$sak_beidz == "2"] <- as.numeric(difftime(x$NDZ_sanemsanas_datums[x$sak_beidz == "2"], prev, units = "days"))
  
  # Ja darba beigšanās datums nozīmē atvaļinājuma vai dīkstāves sākšanos, tad tām dienām atņem 1, 
  # jo to dienu indivīds vairs nestrādā. Tāpat arī, ja cilvēks 
  # (1) atsāk darbu pēc bērnu kopšanas atvaļinājuma un tanī pat dienā aiziet bērnu kopšanas atvaļinājumā (kodi 40 un 41),
  # vai (2) atgriežas darbā pēc bezalgas atvaļinājuma un tanī pat dienā aiziet nākamajā (kodi 50 un 51),
  # tad šo vienu dienu neskaita. Mīnus 1 to nulificēs.
  x$dienas[x$zinkod %in% c("41", "50", "53", "92")] <- x$dienas[x$zinkod %in% c("41", "50", "53", "92")] - 1
  rm(prev)

  #3.3 Kods 26 atceļ iepriekš sarēķinātās dienas darba ņēmēja statusā.
  #    Kods 26: Darba ņēmēja statusa zaudēšana, ja persona nav uzsākusi darbu.
  x$dienas[x$zinkod == "26"] <- 0
  
  return(x)
}



processingDoubles <- function(x, o) {

  x <- x[order(x$PS_code, x$DN_code, x$NM_code, x$NDZ_sanemsanas_datums, x$start, x$end), ]
  temp <- data.frame()
  
  for (k in seq(1, nrow(x), by = 2)) {
    if ((is.na(x$sak)[k] && is.na(x$beidz[k + 1])
         || is.na(x$sak)[k + 1] && is.na(x$beidz[k])) && (x$NDZ_sanemsanas_datums[k] <= x$NDZ_sanemsanas_datums[k+1])) {
      temp <- rbind(temp, x[c(k, k + 1), ])
    }
  }
  rm(k)
  
  if (nrow(temp) == nrow(x)) {
    cat(
      "PĀRBAUDE IZIETA: Visas rindas ir Īstie Divnieki.\n
    )
    rm(temp)
  } else {
    stop(
      cat(
        "ERROR: Visas rindas NAV Īstie Divnieki. Pārskatīt tabulu.\n
      )
    )
  }
  
  #2 Savienojam duplikātu rindas tā, lai sākuma un beigu datumi ir vienā ailē
  x_savienotieDivnieki <- data.frame()
  x <-
    x[order(x$period,
            x$PS_code,
            x$DN_code,
            x$NM_code,
            x$NDZ_sanemsanas_datums, x$start, x$end),]

    for (O in seq(1, nrow(x), by = 2)) {
    y <- x[c(O, O + 1),]
  
    if (y$NDZ_sanemsanas_datums[1] <= y$NDZ_sanemsanas_datums[2] && doublesTest(1, y)) {
      
      #pataisa abas rindas vienādas
      if (y$zinkod[y$end == "2"] == "26") {y$zinkod <- "26"} 
      y$sak[y$end == "2"] <- y$sak[y$start == "1"] 
      y$beidz[y$start == "1"] <- y$beidz[y$end == "2"]
      y$start <- "1"
      y$end <- "2"
      
      #Piešuj pirmo rindu x_savienotieDivnieki tabulai
      x_savienotieDivnieki <- rbind(x_savienotieDivnieki, y[1, ])
    } else {
      stop(cat("Apstrādājot dubultniekus caur processingDouble(), kas nāk no", o, "-nieku apstrādes, dubultnieku sākuma datums NAV agrāks vai vienāds ar beigu datumu. TrueDoubles rindas: ",
               O, "un", O + 1.))
    }
  }
  
  rm(y)
  
  #3 Pārbaudām vai izdalītās rindas sakrīt, vai ailēs [sak] un [beidz] nav NAs.
  
  if (nrow(x_savienotieDivnieki) * 2 == nrow(x)) {
    if (sum(is.na(x_savienotieDivnieki$sak)) == 0) {
      if (sum(is.na(x_savienotieDivnieki$beidz)) == 0) {
        cat(
          "PĀRBAUDE IZIETA:Funkcijā processingDoubles(), jaunizveidotā tabula x_savienotieDivnieki ir izgājusi pārbaudes un tālāk var sarēķināt dienas.\n"
        )
        rm(x)
      } else {
        stop(cat("STOP: Datumu ailē [beidz] ir NA vienības."))
      }
    } else {
      stop(cat("STOP: Datumu ailē [sak] ir NA vienības."))
    }
  } else {
    stop(cat("STOP: No Īsto Divnieku tabulas izvilktāts rindas nesakrīt."))
  }
  
  rm(O)
  
  #4 Tabulu x_savienotieDivnieki apstrādā sekojošos punktos.
  
  x <- x_savienotieDivnieki
  x <- x[order(x$PS_code, x$DN_code, x$NM_code, x$NDZ_sanemsanas_datums, x$start), ]
  rownames(x) <- NULL
  
  #6 Aprēķini dienas no sak līdz beidz un ievieto tās ailē [dienas].

  x$dienas1 <-
    as.numeric(difftime(as.Date(x$beidz), as.Date(x$sak), units = "days")) + 1
  x$dienas <- x$dienas1[(!is.na(x$sak))]
  x$dienas1 <- NULL
  rm(x_savienotieDivnieki)
  
  x <- x[order(x$PS_code, x$DN_code, x$NM_code, x$NDZ_sanemsanas_datums, x$start), ]

  if("40" %in% x$zinkod || "41" %in% x$zinkod || "50" %in% x$zinkod || "51" %in% x$zinkod || "53" %in% x$zinkod || "54" %in% x$zinkod || "91" %in% x$zinkod || "92" %in% x$zinkod) {
    x$dienas <- x$dienas-1
  } else {
    cat("Šajā tabulā nav kodu 40 un 41 vai 50 un 51, vai 53 un 54, vai 91 un 92.")
  }
  
  # Kods 26
  for (b in 1:nrow(x)){
    ifelse(x$zinkod[b] == "26", x$dienas[b] <- 0, x$dienas[b]) 
  }
  rm(b)
  
  return(x)
}

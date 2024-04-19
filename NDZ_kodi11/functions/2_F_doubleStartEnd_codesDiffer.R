F_doubleStartEnd_codesDiffer <- function(x3) {
  rownames(x3) <- NULL
  
  x3 <- x3[order(x3$PS_code, x3$DN_code, x3$NM_code, x3$NDZ_sanemsanas_datums, x3$zinkod), ]
  x3_finish <- data.frame()
  check_rows <- 0
  
  for (k in seq(1, nrow(x3), by = 2)) {
    if(doublesTest(k, x3)) {
      #Ja neviena no sekojošīem kodiem apakštabulā nav, turpini šajā sekcijā, citādi lec uz nākamo.
      if(!("40" %in% x3$zinkod || "41" %in% x3$zinkod || "50" %in% x3$zinkod || "51" %in% x3$zinkod || "53" %in% x3$zinkod || "54" %in% x3$zinkod || "91" %in% x3$zinkod || "92" %in% x3$zinkod)) {
        if (x3$zinkod[k] == '11' && x3$zinkod[k + 1] == '14') {
          x3_finish <- rbind(x3_finish, x3[k + 1,])
          check_rows <- check_rows + 2 
        } else if (x3$zinkod[k] == '14' && x3$zinkod[k + 1] == '11') {
          x3_finish <- rbind(x3_finish, x3[k,])
          check_rows <- check_rows + 2
        } else {
          #print(paste0("Meklējot tabulas x3 rindās kodu 11 un 14 kombinācijas, atzīmēju, ka rindās Nr.", k, " un Nr.", k + 1, " to nav."))
        } 

        if (x3$NDZ_sanemsanas_datums[k] == x3$NDZ_sanemsanas_datums[k + 1]) {
          #Ja datumi neatšķiras, un rinda k ir 21. kods un rinda k+1 ir 25,
          if (x3$zinkod[k] == '21' && x3$zinkod[k + 1] == '25') {
            #tad tabulā x3_finish iet rinda k+1 ar kodu 25, jo tas anulē kodu 21.
            x3_finish <- rbind(x3_finish, x3[k + 1,])
            check_rows <- check_rows + 2
            #Un otrādi.
          } else if (x3$zinkod[k] == '25' && x3$zinkod[k + 1] == '21') {
            #Ja datumi neatšķiras, un rinda k ir 25. kods un rinda k+1 ir 21,
            #tad tabulā x3_finish iet rinda k ar kodu 25, jo tas anulē kodu 21.
            x3_finish <- rbind(x3_finish, x3[k,])
            check_rows <- check_rows + 2
          }
          
          #Taču...
          #ja datumi kodiem atšķiras, tad tik un tā vēlreiz pārbaudam, ka tādi kodi tajās rindās ir.
        } else if (x3$NDZ_sanemsanas_datums[k] > x3$NDZ_sanemsanas_datums[k + 1] && (x3$zinkod[k] == '21' || x3$zinkod[k] == '25') && (x3$zinkod[k + 1] == '21' || x3$zinkod[k + 1] == '25')) {
          #Tabulā x3_finish liksies tā rinda, kurā vēlaks saņemšanas datums.
          x3_finish <- rbind(x3_finish, x3[k,])
          check_rows <- check_rows + 2
        } else if (x3$NDZ_sanemsanas_datums[k] < x3$NDZ_sanemsanas_datums[k + 1]) {
          if ((x3$zinkod[k] == '21' || x3$zinkod[k] == '25') && (x3$zinkod[k + 1] == '21' || x3$zinkod[k + 1] == '25')) {
            x3_finish <- rbind(x3_finish, x3[k + 1,])
            check_rows <- check_rows + 2  
          } 
        } else {
          #print("Šajās rindās nav kodi 21 vai 25.")
        }

        
        if (x3$zinkod[k] == '11' && x3$zinkod[k + 1] == '61') {
          x3_finish <- rbind(x3_finish, x3[k + 1,])
          check_rows <- check_rows + 2
        } else if (x3$zinkod[k] == '61' && x3$zinkod[k + 1] == '11') {
          x3_finish <- rbind(x3_finish, x3[k,])
          check_rows <- check_rows + 2
        } else {
          #print(paste0("Meklējot tabulas x3 rindās kodu 11 un 61 kombinācijas, atzīmēju, ka rindās Nr.", k," un Nr.", k + 1," to nav."))
        }

        if (x3$zinkod[k] == '26' && x3$zinkod[k + 1] == '25' && x3$NDZ_sanemsanas_datums[k] <= x3$NDZ_sanemsanas_datums[k+1]) {
          z <- x3[k, ]
          z$beidz_darbu <- x3$beidz_darbu[k+1]
          z$NDZ_sanemsanas_datums <- x3$NDZ_sanemsanas_datums[k+1]
          x3_finish <- rbind(x3_finish, z)
          rm(z)
          check_rows <- check_rows + 2
        } else if  (x3$zinkod[k] == '25' && x3$zinkod[k + 1] == '26' && x3$NDZ_sanemsanas_datums[k] <= x3$NDZ_sanemsanas_datums[k+1]) {
          x3_finish <- rbind(x3_finish, x3[k + 1,])
          check_rows <- check_rows + 2
        } else {
          #print(paste0("Meklējot tabulas x3 rindās kodu 25 un 26 kombinācijas, atzīmēju, ka rindās Nr.", k," un Nr.", k + 1," to nav."))
        }
      } else {}

      
      if ((x3$zinkod[k] == '40' && x3$zinkod[k + 1] == '41') || (x3$zinkod[k] == '50' && x3$zinkod[k + 1] == '51' || (x3$zinkod[k] == '53' && x3$zinkod[k + 1] == '54') || (x3$zinkod[k] == '91' && x3$zinkod[k + 1] == '92'))&& (x3$NDZ_sanemsanas_datums[k] != x3$NDZ_sanemsanas_datums[k+1])) {
        x3_finish <- rbind(x3_finish, x3[c(k, k + 1),])
        check_rows <- check_rows + 2
      } else {
        # Nezinu ko ielikt
      }
      
     
      
      if (x3$NDZ_sanemsanas_datums[k] == x3$NDZ_sanemsanas_datums[k + 1]) {
        #Ja datumi neatšķiras, un rinda k ir 91. kods un rinda k+1 ir 40,
        if (x3$zinkod[k] == '91' && x3$zinkod[k + 1] == '40') {
          #tad tabulā x3_finish iet rinda k+1 ar kodu 40, jo tas anulē kodu 91.
          x3_finish <- rbind(x3_finish, x3[k + 1,])
          check_rows <- check_rows + 2
          #Un otrādi.
        } else if (x3$zinkod[k] == '40' && x3$zinkod[k + 1] == '91') {
          #Ja datumi neatšķiras, un rinda k ir 40. kods un rinda k+1 ir 91,
          #tad tabulā x3_finish iet rinda k ar kodu 40, jo tas anulē kodu 91.
          x3_finish <- rbind(x3_finish, x3[k,])
          check_rows <- check_rows + 2
        }#Taču...
        #ja datumi kodiem atšķiras, tad tik un tā vēlreiz pārbauda, ka tādi kodi tajās rindās ir.
      } else if (x3$NDZ_sanemsanas_datums[k] > x3$NDZ_sanemsanas_datums[k + 1] && (x3$zinkod[k] == '91' || x3$zinkod[k] == '40') && (x3$zinkod[k + 1] == '91' || x3$zinkod[k + 1] == '40')) {
        #Tabulā x3_finish liksies tā rinda, kurā vēlaks saņemšanas datums.
        x3_finish <- rbind(x3_finish, x3[k,])
        check_rows <- check_rows + 2
      } else if ((x3$NDZ_sanemsanas_datums[k] < x3$NDZ_sanemsanas_datums[k + 1]) &&
                 (x3$zinkod[k] == '40' || x3$zinkod[k] == '91') && (x3$zinkod[k + 1] == '40' || x3$zinkod[k + 1] == '91')) {
        x3_finish <- rbind(x3_finish, x3[k + 1,])
        check_rows <- check_rows + 2
      } else {
        #print("Šajās rindās nav kodi 40 vai 91.")
      }
      

      
      if (x3$NDZ_sanemsanas_datums[k] == x3$NDZ_sanemsanas_datums[k + 1]) {
        #Ja datumi neatšķiras, un rinda k ir 41. kods un rinda k+1 ir 92,
        if (x3$zinkod[k] == '41' && x3$zinkod[k + 1] == '92') {
          #tad tabulā x3_finish iet rinda k ar kodu 41, jo tas anulē kodu 92.
          x3_finish <- rbind(x3_finish, x3[k,])
          check_rows <- check_rows + 2
          #Un otrādi.
        } else if (x3$zinkod[k] == '92' && x3$zinkod[k + 1] == '41') {
          #Ja datumi neatšķiras, un rinda k+1 ir 41. kods un rinda k ir 92,
          #tad tabulā x3_finish iet rinda k+1 ar kodu 41, jo tas anulē kodu 92.
          x3_finish <- rbind(x3_finish, x3[k+1,])
          check_rows <- check_rows + 2
        }#Taču... 
        #ja datumi kodiem atšķiras, tad tik un tā vēlreiz pārbauda, ka tādi kodi tajās rindās ir.
      } else if (x3$NDZ_sanemsanas_datums[k] < x3$NDZ_sanemsanas_datums[k + 1] && 
                 (x3$zinkod[k] == '41' || x3$zinkod[k] == '92') && (x3$zinkod[k + 1] == '92' || x3$zinkod[k + 1] == '41')) {
        #Tabulā x3_finish liksies tā rinda, kurā vēlaks saņemšanas datums.
        x3_finish <- rbind(x3_finish, x3[k+1,])
        check_rows <- check_rows + 2
      } else if ((x3$NDZ_sanemsanas_datums[k] > x3$NDZ_sanemsanas_datums[k + 1]) &&
                 (x3$zinkod[k] == '41' || x3$zinkod[k] == '92') && (x3$zinkod[k + 1] == '92' || x3$zinkod[k + 1] == '41')) {
        x3_finish <- rbind(x3_finish, x3[k,])
        check_rows <- check_rows + 2 
      } else {
        #print("Šajās rindās nav kodi 40 vai 91.")
      }
    } else {
      stop(cat("ERROR: DoublesTest nav iziets. Rinda:", k))
    }
  }
  
  
  # Gala pārbaude
  
  rownames(x3_finish) <- NULL
  if (nrow(x3) != check_rows) {
    stop(cat("ERROR: Atvasināto tabulu rindas nesakrīt ar mātes tabulu."))
  } else {
    rm(check_rows, k, x3)
    return(x3_finish)
  }
}

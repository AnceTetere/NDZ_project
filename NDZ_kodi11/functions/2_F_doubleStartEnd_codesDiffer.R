# Šis būs ļoti hard-coded:


#GADĪJUMS 1:
F_doubleStartEnd_codesDiffer <- function(x3) {
  rownames(x3) <- NULL
  x3_finish <- data.frame()

 if(!("40" %in% x3$zinkod) || !("41" %in% x3$zinkod)) {
   for (k in seq(1, nrow(x3), by = 2)) {
     if(doublesTest(k, x3)) {
       if (x3$zinkod[k] == '11' && x3$zinkod[k + 1] == '14') {
         x3_finish <- rbind(x3_finish, x3[k + 1,])
       } else if (x3$zinkod[k] == '14' && x3$zinkod[k + 1] == '11') {
         x3_finish <- rbind(x3_finish, x3[k,])
       } else {
         print(
           paste0(
             "Meklējot tabulas x3 rindās kodu 11 un 14 kombinācijas, atzīmēju, ka rindās Nr.",
             k,
             " un Nr.",
             k + 1,
             " to nav."
           )
         )
       } 
     } else {
       cat("Obligātie rindu kodi nesakrīt:")
       print(x3[c(k, k+1), ])
     }
   }
   
   for (k in seq(1, nrow(x3), by = 2)) {
     if(doublesTest(k, x3)) {
       if (x3$NDZ_sanemsanas_datums[k] == x3$NDZ_sanemsanas_datums[k + 1]) {
         #Ja datumi neatšķiras, un rinda k ir 21. kods un rinda k+1 ir 25,
         if (x3$zinkod[k] == '21' && x3$zinkod[k + 1] == '25') {
           #tad tabulā x3_finish iet rinda k+1 ar kodu 25, jo tas anulē kodu 21.
           x3_finish <- rbind(x3_finish, x3[k + 1,])
           #Un otrādi.
         } else if (x3$zinkod[k] == '25' && x3$zinkod[k + 1] == '21') {
           #Ja datumi neatšķiras, un rinda k ir 25. kods un rinda k+1 ir 21,
           #tad tabulā x3_finish iet rinda k ar kodu 25, jo tas anulē kodu 21.
           x3_finish <- rbind(x3_finish, x3[k,])
         }
         
         #Taču...
       } else if ((x3$NDZ_sanemsanas_datums[k] > x3$NDZ_sanemsanas_datums[k + 1]) &&
                  #ja datumi kodiem atšķiras, tad tik un tā vēlreiz pārbaudam, ka tādi kodi tajās rindās ir.
                  (x3$zinkod[k] == '21' || x3$zinkod[k] == '25') &&
                  (x3$zinkod[k + 1] == '21' || x3$zinkod[k + 1] == '25')) {
         #Tabulā x3_finish liksies tā rinda, kurā vēlaks saņemšanas datums.
         x3_finish <- rbind(x3_finish, x3[k,])
       } else if ((x3$NDZ_sanemsanas_datums[k] < x3$NDZ_sanemsanas_datums[k + 1]) &&
                  (x3$zinkod[k] == '21' || x3$zinkod[k] == '25') &&
                  (x3$zinkod[k + 1] == '21' || x3$zinkod[k + 1] == '25')) {
         x3_finish <- rbind(x3_finish, x3[k + 1,])
       } else {
         print("Šajās rindās nav kodi 21 vai 25.")
       }
     } else {
       cat("Obligātie rindu kodi nesakrīt:")
       print(x3[c(k, k+1), ])
     }
   }
   
   
   for (k in seq(1, nrow(x3), by = 2)) {
     if(doublesTest(k, x3)) {
       if (x3$zinkod[k] == '11' && x3$zinkod[k + 1] == '61') {
         x3_finish <- rbind(x3_finish, x3[k + 1,])
       } else if (x3$zinkod[k] == '61' && x3$zinkod[k + 1] == '11') {
         x3_finish <- rbind(x3_finish, x3[k,])
       } else {
         print(
           paste0(
             "Meklējot tabulas x3 rindās kodu 11 un 61 kombinācijas, atzīmēju, ka rindās Nr.",
             k,
             " un Nr.",
             k + 1,
             " to nav."
           )
         )
       }
     } else {
       cat("Obligātie rindu kodi nesakrīt:")
       print(x3[c(k, k+1), ])
     }
   }
   
   rownames(x3_finish) <- NULL
   
   #pārbaude
   if (nrow(x3) / nrow(x3_finish) == 2) {
     return(x3_finish)
   } else {
     stop(return(cat("ERROR: Atvasināto tabulu rindas nesakrīt ar mātes tabulu.")))
   }
   
 } else {#GADĪJUMS 4:
   
   for (k in seq(1, nrow(x3), by = 2)) {
     if(doublesTest(k, x3)) {
       if ((x3$zinkod[k] == '40' && x3$zinkod[k + 1] == '41') && (x3$NDZ_sanemsanas_datums[k] != x3$NDZ_sanemsanas_datums[k+1])) {
         x3_finish <- rbind(x3_finish, x3[c(k, k + 1),])
       } else {}
     } else {
       cat("Obligātie rindu atribūti nesakrīt:")
       print(x3[c(k, k+1), ])
     }
   }
   
   rownames(x3_finish) <- NULL
   
   #pārbaude
   if (nrow(x3) == nrow(x3_finish)) {
     return(x3_finish)
   } else {
     stop(return(cat("ERROR: Atvasināto tabulu rindas nesakrīt ar mātes tabulu.")))
   }
 }
  
if (((x3$zinkod[k] == '11' && x3$zinkod[k + 1] == '14') || (x3$zinkod[k] == '14' && x3$zinkod[k + 1] == '11') && ((x3$zinkod[k] == '21' && x3$zinkod[k + 1] == '25') || (x3$zinkod[k] == '25' && x3$zinkod[k + 1] == '21'))) || ((((x3$NDZ_sanemsanas_datums[k] > x3$NDZ_sanemsanas_datums[k + 1]) && ((x3$zinkod[k] == '21' || x3$zinkod[k] == '25') && (x3$zinkod[k + 1] == '21' || x3$zinkod[k + 1] == '25'))) || ((x3$NDZ_sanemsanas_datums[k] < x3$NDZ_sanemsanas_datums[k + 1]) && ((x3$zinkod[k] == '21' || x3$zinkod[k] == '25') && (x3$zinkod[k + 1] == '21' || x3$zinkod[k + 1] == '25')))) || (((x3$zinkod[k] == '11' && x3$zinkod[k + 1] == '61') || (x3$zinkod[k] == '61' && x3$zinkod[k + 1] == '11')) || ((x3$zinkod[k] == '40' && x3$zinkod[k + 1] == '41') && (x3$NDZ_sanemsanas_datums[k] != x3$NDZ_sanemsanas_datums[k+1]))))) {
  cat("F_doubleStartEnd_codesDiffer algoritmiskā izstrādes funkcija veiksmīgi pabeigta.")
} else {
  stop(cat("TRŪKST APSTRĀDES KODA: F_doubleStartEnd_codesDiffer algoritmiskās izstrādes funkcijā"))
}
  
}

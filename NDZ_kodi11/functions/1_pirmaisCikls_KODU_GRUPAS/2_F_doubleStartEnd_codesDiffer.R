F_doubleStartEnd_codesDiffer <- function(x3) {
  rownames(x3) <- NULL
  x3 <- x3[order(x3$PS_code, x3$DN_code, x3$NM_code, x3$ZDN_sanemsanas_datums, x3$zinkod), ]
  
  x3_finish <- data.frame()
  check_rows <- 0 
#k <- 1
for (k in seq(1, nrow(x3), by = 2)) {
    if(doublesTest(k, x3)) {
      if(!any(x3$zinkod %in% c("40", "41", "50", "51", "53", "54", "91", "92"))) {
        
        #GADĪJUMS 1:
        if ((x3$zinkod[k] == '11' && x3$zinkod[k + 1] %in% c('14', '16')) || 
            (x3$zinkod[k] %in% c('14', '16') && x3$zinkod[k + 1] == '11')) {
          x3_finish <- rbind(x3_finish, x3[k + (x3$zinkod[k] == '11'),])
        
        #GADĪJUMS 2:
        } else if ((x3$zinkod[k] == '21' && x3$zinkod[k + 1] %in% c('25', '29')) ||
            (x3$zinkod[k] %in% c('25', '29') && x3$zinkod[k + 1] == '21')){
          if (diff(x3$ZDN_sanemsanas_datums[k:(k + 1)]) == 0) {
              x3_finish <- rbind(x3_finish, x3[k + (x3$zinkod[k] == '21'),])
            }  else if (diff(x3$ZDN_sanemsanas_datums[k:(k+1)]) < 0) {
              x3_finish <- rbind(x3_finish, x3[k,])
            } else if (diff(x3$ZDN_sanemsanas_datums[k:(k+1)]) > 0) {
              x3_finish <- rbind(x3_finish, x3[k + 1,])
            }
          
      #GADĪJUMS 3:
      
      } else if ((x3$zinkod[k] == '24' && x3$zinkod[k + 1] == '25') ||
          (x3$zinkod[k] == '25' && x3$zinkod[k + 1] == '24')){
        if (diff(x3$ZDN_sanemsanas_datums[k:(k + 1)]) == 0) {
          #Ja datumi neatšķiras, un vienā rinda ir 24. kods un otrā rindā ir 25,
          #tad tabulā x3_finish iet rinda ar kodu 24, jo tas anulē kodu 25.
          x3_finish <- rbind(x3_finish, x3[k + (x3$zinkod[k] == '25'),])
        #Taču ja datumi atšķiras
        } else if (diff(x3$ZDN_sanemsanas_datums[k:(k+1)]) < 0) {
          #Tabulā x3_finish liksies tā rinda, kurā vēlaks saņemšanas datums.
          x3_finish <- rbind(x3_finish, x3[k,])
        } else if (diff(x3$ZDN_sanemsanas_datums[k:(k+1)]) > 0) {
          x3_finish <- rbind(x3_finish, x3[k + 1,])
        } 

      #GADĪJUMS 4:
      
      } else if ((x3$zinkod[k] == '22' && x3$zinkod[k + 1] == '25') ||
          (x3$zinkod[k] == '25' && x3$zinkod[k + 1] == '22')){
        if (diff(x3$ZDN_sanemsanas_datums[k:(k + 1)]) == 0) {
          #Ja datumi neatšķiras, un vienā rinda ir 22. kods un otrā rindā ir 25,
          #tad tabulā x3_finish iet rinda ar kodu 22, jo tas anulē kodu 25.
          x3_finish <- rbind(x3_finish, x3[k + (x3$zinkod[k] == '25'),])
        #Taču ja datumi atšķiras
        } else if (diff(x3$ZDN_sanemsanas_datums[k:(k+1)]) < 0) {
          #Tabulā x3_finish liksies tā rinda, kurā vēlaks saņemšanas datums.
          x3_finish <- rbind(x3_finish, x3[k,])
        } else if (diff(x3$ZDN_sanemsanas_datums[k:(k+1)]) > 0) {
          x3_finish <- rbind(x3_finish, x3[k + 1,])
        } 

      #GADĪJUMS 5:
        
        } else if ((x3$zinkod[k] == '25' && x3$zinkod[k + 1] == '29') ||
            (x3$zinkod[k] == '29' && x3$zinkod[k + 1] == '25')) {
          if (diff(x3$ZDN_sanemsanas_datums[k:(k + 1)]) == 0) {
          #Ja datumi neatšķiras, un vienā rinda ir 25. kods un otrā rindā ir 29,
            #tad tabulā x3_finish iet rinda ar kodu 29, jo tas anulē kodu 25.
            x3_finish <- rbind(x3_finish, x3[k + (x3$zinkod[k] == '25'),])
          #Taču ja datumi atšķiras
           } else if (diff(x3$ZDN_sanemsanas_datums[k:(k+1)]) < 0) {
          #Tabulā x3_finish liksies tā rinda, kurā vēlaks saņemšanas datums.
          x3_finish <- rbind(x3_finish, x3[k,])
          } else if (diff(x3$ZDN_sanemsanas_datums[k:(k+1)]) > 0) {
          x3_finish <- rbind(x3_finish, x3[k + 1,])
          } 

      #GADĪJUMS 6:
      
      } else if ((x3$zinkod[k] == '23' && x3$zinkod[k + 1] %in% c('24', '25', '29')) ||
          (x3$zinkod[k] %in% c('24','25', '29') && x3$zinkod[k + 1] == '23')){
        if (diff(x3$ZDN_sanemsanas_datums[k:(k + 1)]) == 0) {
          #Ja datumi neatšķiras, un vienā rinda ir 23. kods un otrā rindā ir 24, 25 vai 29,
          #tad tabulā x3_finish iet rinda ar kodu 23, jo tas anulē kodus 24, 25 un 29.
          x3_finish <- rbind(x3_finish, x3[k + (x3$zinkod[k] %in% c('24','25','29')),])
        #Taču ja datumi atšķiras
        } else if (diff(x3$ZDN_sanemsanas_datums[k:(k+1)]) < 0) {
          #Tabulā x3_finish liksies tā rinda, kurā vēlaks saņemšanas datums.
          x3_finish <- rbind(x3_finish, x3[k,])
        } else if (diff(x3$ZDN_sanemsanas_datums[k:(k+1)]) > 0) {
          x3_finish <- rbind(x3_finish, x3[k + 1,])
        } 

        #GADĪJUMS 7:
        
        } else if ((x3$zinkod[k] == '11' && x3$zinkod[k + 1] == '61') ||
                   (x3$zinkod[k] == '61' && x3$zinkod[k + 1] == '11')){
                    x3_finish <- rbind(x3_finish, x3[k + (x3$zinkod[k] == '11'),])

        #GADĪJUMS 8:

        } else if ((x3$zinkod[k] == '26' && x3$zinkod[k + 1] == '25') ||
                   (x3$zinkod[k] == '25' && x3$zinkod[k + 1] == '26')) {
               if (diff(x3$ZDN_sanemsanas_datums[k:(k+1)]) <= 0) {
                   z <- x3[k + (x3$zinkod[k] == '25'), ]
                   z$ZDN_sanemsanas_datums <- x3$ZDN_sanemsanas_datums[k + (x3$zinkod[k] == '25')]
                   x3_finish <- rbind(x3_finish, z)
                   rm(z)
                   }

      #GADĪJUMS 9:
      
      }
        } else {
            
        valid_pairs <- list(c('40', '41'), c('50', '51'), c('53', '54'), c('91', '92'))
        pair_check <- any(sapply(valid_pairs, function(pair) all(x3$zinkod[k:(k + 1)] == pair)))
      
         if (pair_check && diff(x3$ZDN_sanemsanas_datums[k:(k + 1)]) != 0) {
           x3_finish <- rbind(x3_finish, x3[k : (k + 1),])
           rm(check_rows, k, x3, valid_pairs, pair_check)

      #GADĪJUMS 10:
         } else if (x3$zinkod[k] %in% c('91','40') && x3$zinkod[k + 1] %in% c('91', '40')) {
           if (diff(x3$ZDN_sanemsanas_datums[k:(k+1)]) == 0) {
             x3_finish <- rbind(x3_finish, x3[k + (x3$zinkod[k] == "91"),])
           } else if (diff(x3$ZDN_sanemsanas_datums[k:(k+1)]) < 0) {
             x3_finish <- rbind(x3_finish, x3[k,])
           } else if (diff(x3$ZDN_sanemsanas_datums[k:(k+1)]) > 0) {
             x3_finish <- rbind(x3_finish, x3[k + 1,])
           }

      #GADĪJUMS 11:

         } else if (x3$zinkod[k] %in% c('92','41') && x3$zinkod[k + 1] %in% c('92', '41')) {
           if (diff(x3$ZDN_sanemsanas_datums[k:(k+1)]) == 0) {
             x3_finish <- rbind(x3_finish, x3[k + (x3$zinkod[k] == '92'),])
           } else if (diff(x3$ZDN_sanemsanas_datums[k:(k+1)]) > 0) {
             x3_finish <- rbind(x3_finish, x3[k+1,])
           } else if (diff(x3$ZDN_sanemsanas_datums[k:(k+1)]) < 0) {
             x3_finish <- rbind(x3_finish, x3[k,])
           } 
         }
        }
         check_rows <- check_rows + 2
    } else {
        stop("ERROR: Funkcijā F_doubleStartEnd_codesDiffer DoublesTest nav iziets. Rinda: ", k)
      } 
    } 
      
  # Gala pārbaude
  
  rownames(x3_finish) <- NULL
  if (nrow(x3) != check_rows) {
    stop("ERROR: Funkcijā F_doubleStartEnd_codesDiffer atvasināto tabulu rindas nesakrīt ar mātes tabulu.")
  } else {
    rm(check_rows, k, x3)
    return(x3_finish)
  }
}

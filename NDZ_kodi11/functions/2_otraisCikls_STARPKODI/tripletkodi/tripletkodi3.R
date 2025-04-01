tripletkodi3 <- function(y3, t, prev, v) {
  
  yt <- y3[v, ]
  
  if (t$zinkod[1] %in% c("41", "51", "54", "92")) {
            if (t$zinkod[2] %in% c("40", "50", "53", "91")) {
              if (t$zinkod[3] %in% c("21", "22", "23", "24", "25", "29")) {
                if (diff(t$NDZ_sanemsanas_datums[1:2]) == 0 && diff(t$NDZ_sanemsanas_datums[2:3]) != 0) {
                  yt$dienas <- 1
                } else if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
                  yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[1:2]))
                } else {stop("Tripletkodi3 iztrūkst apstrādes koda.")}
                } else {stop("Tripletkodi3 iztrūkst apstrādes koda.")}
              } else if (t$zinkod[2] %in% c("21", "22", "23", "24", "25", "29")) {
                   if (t$zinkod[3] %in% c("40", "50", "53", "91")) {
                      if (diff(t$NDZ_sanemsanas_datums[1:2]) != 0 && diff(t$NDZ_sanemsanas_datums[2:3]) == 0) {
                       yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[1:2]))
                     } else {stop("Tripletkodi3 iztrūkst apstrādes koda.")}
                  } else {stop("Tripletkodi3 iztrūkst apstrādes koda.")}
               } else if (t$zinkod[2] %in% c("41", "51", "54", "92")) {
                       if (t$zinkod[3] %in% c("21", "22", "23", "24", "25", "29")) {
                         if (diff(t$NDZ_sanemsanas_datums[1:2]) == 0 && diff(t$NDZ_sanemsanas_datums[2:3]) != 0) {
                           if (t$period[1] == '_____' && t$PS_code[1] == '_____' && t$NM_code[1] == '_____') {
                             yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[c(1,3)]))
                           } else {stop("Tripletkodi3 iztrūkst apstrādes koda.")}
                         } else {stop("Tripletkodi3 iztrūkst apstrādes koda.")}
                       } else {stop("Tripletkodi3 iztrūkst apstrādes koda.")}
                } else {stop("Tripletkodi3 iztrūkst apstrādes koda.")}
    } else if (t$zinkod[1] %in% c("21", "22", "23", "24", "25", "29")) {
               if (t$zinkod[2] %in% c("41", "51", "54", "92")) {
                 if (t$zinkod[3] %in% c("40", "50", "53", "91")) {
                   if (all(diff(t$NDZ_sanemsanas_datums) == 0)) {
                     yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[1:2]))
                   } else {stop("Tripletkodi3 iztrūkst apstrādes koda.")}
                 } else if (t$zinkod[3] %in% c("41", "51", "54", "92")) {
                   if (all(diff(t$NDZ_sanemsanas_datums) == 0)) {
                     yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[1:2]))
                     ZERO_minus(t %>% slice(2:3))
                   } else {stop("Tripletkodi3 iztrūkst apstrādes koda.")}
                 } else {stop("Tripletkodi3 iztrūkst apstrādes koda.")}
               } else {stop("Tripletkodi3 iztrūkst apstrādes koda.")}
     } else if (t$zinkod[1] %in% c("11", "14", "16", "61")) {
             if (t$zinkod[2] %in% c("40", "50", "53", "91")) {
                 if (t$zinkod[3] %in% c("40", "50", "53", "91")) {
                  if (all(diff(t$NDZ_sanemsanas_datums) == 0)) {
                    yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[c(1,3)]))
                  } else {stop("Tripletkodi3 iztrūkst apstrādes koda.")}
                } else {stop("Tripletkodi3 iztrūkst apstrādes koda.")}
            } else if (t$zinkod[2] %in% c("41", "51", "54", "92")) {
                if (t$zinkod[3] %in% c("40", "50", "53", "91")) {
                  if (diff(t$NDZ_sanemsanas_datums[2:3]) == 0 && diff(t$NDZ_sanemsanas_datums[1:2]) != 0) {
                    if (t$PS_code[1] == '_____' && t$NM_code[1] == '_____') {
                      yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[c(1,3)]))
                    } else {stop("Tripletkodi3 iztrūkst apstrādes koda.")}
                  } else if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
                    if (t$PS_code[1] == '_____' && t$NM_code[1] == '_____') {
                      yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[c(1,3)]))
                    } else {stop("Tripletkodi3 iztrūkst apstrādes koda.")}
                  } else {stop("Tripletkodi3 iztrūkst apstrādes koda.")}
               } else {stop("Tripletkodi3 iztrūkst apstrādes koda.")}
              } else {stop("Tripletkodi3 iztrūkst apstrādes koda.")}
     } else if (t$zinkod[1] %in% c("40", "50", "53", "91")) {
             if (t$zinkod[2] %in% c("41", "51", "54", "92")) {
              if (t$zinkod[3] %in% c("21", "22", "23", "24", "25", "29")) {
               if (diff(t$NDZ_sanemsanas_datums[2:3]) != 0 && diff(t$NDZ_sanemsanas_datums[1:2]) == 0) {
                 if (t$period[1] == "_____" && t$PS_code[1] == "_____" && t$NM_code[1] == "_____") {
                   yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[1:2]))
                 } else {stop("Tripletkodi3 iztrūkst apstrādes koda.")}
               } else {stop("Tripletkodi3 iztrūkst apstrādes koda.")}
             } else {stop("Tripletkodi3 iztrūkst apstrādes koda.")}
           } else {stop("Tripletkodi3 iztrūkst apstrādes koda.")}
          } else {stop("Tripletkodi3 iztrūkst apstrādes koda.")}

  rm(y3, t, prev, v)
  if(is.na(yt$PS_code[1])) {stop("Dienas NA.")}
  yt$zinkod <- "combined"  #jo tripletkodu dienu sarēķins
  return(yt)
}

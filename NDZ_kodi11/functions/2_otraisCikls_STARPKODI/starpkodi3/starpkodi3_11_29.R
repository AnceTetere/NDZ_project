starpkodi3_11_29 <- function(y, t, prev, v) {
  
  yt <- y[v,]
  
  if (all(diff(t$NDZ_sanemsanas_datums) == 0)) {
                  if (t$zinkod[3] %in% c("40", "50", "53", "91")) {
                    if (t$period[1] == "______" && t$PS_code[1] %in% c('______', '______') && 
                        t$NM_code[1] %in% c('______', '______')) {
                      yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[1:2]))
                    } else if (t$period[1] == "______" && 
                               t$PS_code[1] %in% c('_________', '_____', '__________', '_____', '_____', '_____') && 
                               t$NM_code[1] %in% c('__________', '__________', '__________', '__________', '__________', '__________')) {
                      yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[c(1,3)]))
                    }  else if (t$period[1] == "______" && t$PS_code[1] == '_________' && t$NM_code[1] == '__________') {
                      yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[1:2]))
                    } else {stop("Starpkodi3_11: Trūkst izstrādes koda. \n
                  } else {stop("Starpkodi3_11: Trūkst izstrādes koda.")}
  } else if (diff(t$NDZ_sanemsanas_datums[1:2]) == 0 && diff(t$NDZ_sanemsanas_datums[2:3]) != 0) {
                 if (t$zinkod[3] %in% c("40", "50", "53", "91")) {
                   if (t$period[1] == "______" && t$PS_code[1] == '_________' && t$NM_code[1] == '__________') {
                     yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[1:2]))
                   } else if (t$period[1] == "______" && t$PS_code[1] == '_________' && t$NM_code[1] == '__________') {
                     yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[1:2]))
                   } else {stop("Starpkodi3_11: Trūkst izstrādes koda.")}
                 } else {stop("Starpkodi3_11: Trūkst izstrādes koda.")}
  } else if (diff(t$NDZ_sanemsanas_datums[1:2]) != 0 && diff(t$NDZ_sanemsanas_datums[2:3]) == 0) {
               if (t$zinkod[3] %in% c("40", "50", "53", "91")) {
                 if ((t$period[1] == "_____" && t$PS_code[1] == '_________' && t$NM_code[1] == '__________') ||
                     (t$period[1] == "______" && t$PS_code[1] == '__________' && t$NM_code[1] == '__________') ||
                     (t$period[1] == "_____" && t$PS_code[1] == '_________' && t$NM_code[1] == '__________') ||
                     (t$period[1] == "_____" && t$PS_code[1] == '_________ && t$NM_code[1] == '__________') ||
                     (t$period[1] == "______" && t$PS_code[1] == '_________' && t$NM_code[1] == '__________')) {
                   yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[1:2]))
                 } else {stop("Starpkodi3_11: Trūkst izstrādes koda.")}
               } else if (t$zinkod[3] %in% c("41", "51", "54", "92")) {
                 if (t$period[1] == "______" && t$PS_code[1] == '__________' && t$NM_code[1] == '__________') {
                   yt$dienas <- as.numeric(diff(t$NDZ_sanemsanas_datums[1:2]))
                 } else {stop("Starpkodi3_11: Trūkst izstrādes koda.")}
               } else {stop("Starpkodi3_11: Trūkst izstrādes koda.")}
  } else {stop("Starpkodi3_11: Trūkst izstrādes koda.")}

  rm(y, t, prev, v)
  return(yt)
}

splittingThrees <- function(x3) {
  
  x_vieninieki <- data.frame()
  x_divnieki <- data.frame()
  
  #if (all(diff(x3$NDZ_sanemsanas_datums) == 0) && all(x3$sak_beidz[c(1,3)] == "1") && x3$sak_beidz[2] == "2" && 
  #    x3$zinkod[1] %in% c("92", "41") && x3$zinkod[3] %in% c("92", "41") && x3$PS_code[1] == '__________' && x3$NM_code[1] == '_________'){
  #  x_vieninieki <- rbind(x_vieninieki, x3[3, ]) 
  #} else if ((x3$zinkod[1] == "40" && x3$zinkod[2] == "41" && x3$zinkod[3] == "41") && (x3$NDZ_sanemsanas_datums[2] <= x3$NDZ_sanemsanas_datums[3]) && (x3$NDZ_sanemsanas_datums[1] != x3$NDZ_sanemsanas_datums[2])) {
  #  x_vieninieki <- rbind(x_vieninieki, x3[c(1, 3), ]) 
    # vv || (x3$sak_beidz[1] == "2" && x3$sak_beidz[2] == "1" && x3$sak_beidz[3] != "2" && diff(x3$NDZ_sanemsanas_datums[1:2]) == 0 && diff(x3$NDZ_sanemsanas_datums[2:3]) != 0)) {

  #} else 
    
  if (x3$sak_beidz[1] == "1" && all(x3$sak_beidz[2:3] == "2") && all(x3$zinkod != '26') && diff(x3$NDZ_sanemsanas_datums[1:2]) >= 0 && diff(x3$NDZ_sanemsanas_datums[2:3]) >= 0) {
    x_divnieki <- rbind(x_divnieki, x3[c(1,3), ])
  } else if (all(x3$sak_beidz[c(1,3)] == "1") && x3$sak_beidz[2] == "2" && diff(x3$NDZ_sanemsanas_datums[1:2]) >= 0 && diff(x3$NDZ_sanemsanas_datums[2:3]) > 0) {
    x_divnieki <- rbind(x_divnieki, x3[1:2, ])
    x_vieninieki <- rbind(x_vieninieki, x3[3, ])    
  } else if (x3$sak_beidz[2] == "1" && all(x3$sak_beidz[c(1,3)] == "2") && diff(x3$NDZ_sanemsanas_datums[1:2]) != 0) {
    x_divnieki <- rbind(x_divnieki, x3[2:3, ])
    x_vieninieki <- rbind(x_vieninieki, x3[1, ])
  } else if (all(x3$sak_beidz[1:2] == "2") && all(x3$zinkod != "26") && x3$sak_beidz[3] == "1") {
    x_divnieki <- rbind(x_divnieki, x3[2:3, ])
  } else if (all(x3$sak_beidz[1:2] == "2") && any(x3$zinkod %in% "26") && x3$sak_beidz[3] == "1") {
    x_divnieki <- rbind(x_divnieki, x3[x3$sak_beidz == "1", ], x3[x3$zinkod == "26", ])
  } else if (x3$sak_beidz[1] == "1" && all(x3$sak_beidz[2:3] == "2") && any(x3$zinkod %in% '26') && 
             diff(x3$NDZ_sanemsanas_datums[1:2]) >= 0 && 
             diff(x3$NDZ_sanemsanas_datums[2:3]) >= 0) {
    x_divnieki <- rbind(x_divnieki, x3[x3$sak_beidz == "1", ], x3[x3$zinkod == "26", ][nrow(x3[x3$zinkod == "26", ]),])
    #} else if ((x3$sak_beidz[1] == "2" && x3$sak_beidz[2] == "1" && x3$sak_beidz[3] == "1") && (x3$NDZ_sanemsanas_datums[1] != x3$NDZ_sanemsanas_datums[3])) {
    #  x_vieninieki <- rbind(x_vieninieki, x3[c(1, 3), ])
    #} else if (all(x3$sak_beidz[c(1,3)] == "1") && x3$sak_beidz[2] == "2" &&
    #           diff(x3$NDZ_sanemsanas_datums[1:2]) != 0 && diff(x3$NDZ_sanemsanas_datums[2:3]) == 0) {                                                    
    #  x_divnieki <- rbind(x_divnieki, x3[2:3, ])
    # } else if (all(x3$sak_beidz == "2") && all(diff(x3$NDZ_sanemsanas_datums) == 0)) {                                                    
    #  x_vieninieki <- rbind(x_vieninieki, x3[1, ])
  } else {
    stop("No trijniekiem sūtītajā tabulā uz splittingThrees trūkst apstrādes koda!")
  }

  return(list(x_vieninieki = x_vieninieki, x_divnieki = x_divnieki))
}

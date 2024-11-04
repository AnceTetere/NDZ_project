starpkodi3 <- function(y, t, prev, v) {
# for tests y <- y2
  
  if (t$zinkod[1] == "11") {
    yt <- starpkodi3_11(y, t, prev, v)
  } else if (t$zinkod[1] %in% c("41", "51", "54", "92")) {
    yt <- starpkodi3_51(y, t, prev, v)
  } else if (t$zinkod[1] == "50"){
     yt <- starpkodi3_50(y, t, prev, v)
  } else if (t$zinkod[1] %in% c("21", "25")) {
     yt <- starpkodi3_25(y, t, prev, v)
#  } else if (t$zinkod[1] == "53") {  
#    yt <- starpkodi3_53(y2, t, prev, v)
  } else if (t$zinkod[1] == "91") {  
    yt <- starpkodi3_91(y, t, prev, v)
    
#  } else if (t$zinkod[1] == "21") {  
#    yt <- starpkodi3_21(y2, t, prev, v)
#  } else if (t$zinkod[1] == "40") {  
#    yt <- starpkodi3_40(y2, t, prev, v)

#    } else if (t$zinkod[1] == "21" && t$zinkod[2] == "51" && t$zinkod[3] == "51" && 
#               diff(t$NDZ_sanemsanas_datums[1:2]) == 0) {

#      yt <- y2[v, ]
#      yt$dienas <- 0
    } else {stop("Starpkodi3: Trūkst izstrādes koda.")}
  
  if(is.na(yt$PS_code[1])) {stop("Dienas NA.")}
  yt$zinkod <- "combined"  #jo starpkodu dienu sarēķins
  return(yt)
}

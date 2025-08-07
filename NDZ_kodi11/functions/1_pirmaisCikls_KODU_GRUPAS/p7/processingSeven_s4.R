processingSeven_s4 <- function(x7s4, o, kods) {
  x7s4_uzVieniniekiem <- data.frame(); x7s4_uzDivniekiem <- data.frame(); x7s4_uzTrijniekiem <- data.frame(); x7s4_uzCetriniekiem <- data.frame(); x7s4_uzPieciniekiem <- data.frame(); x7s4_uzSesiniekiem <- data.frame()
  #x7s4 <- x7
  
  x7s4 <- x7s4 %>% dplyr::arrange(NDZ_sanemsanas_datums)
  
  result <- function(y) {
    x7s4_uzVieniniekiem <<- rbind(x7s4_uzVieniniekiem, y$x7s4_uzVieniniekiem)
    x7s4_uzDivniekiem   <<- rbind(x7s4_uzDivniekiem, y$x7s4_uzDivniekiem)
    x7s4_uzTrijniekiem  <<- rbind(x7s4_uzTrijniekiem, y$x7s4_uzTrijniekiem)
    x7s4_uzCetriniekiem <<- rbind(x7s4_uzCetriniekiem, y$x7s4_uzCetriniekiem)
    x7s4_uzPieciniekiem <<- rbind(x7s4_uzPieciniekiem, y$x7s4_uzPieciniekiem)
    x7s4_uzSesiniekiem  <<- rbind(x7s4_uzSesiniekiem, y$x7s4_uzSesiniekiem)
    rm(y)
  }
  
  if (all(x7s4$sak_beidz[c(1,3,4,5)] == "1")) {
           result(processingSeven_s4_1345(x7s4, o, kods))
  } else if (all(x7s4$sak_beidz[c(1,3,4,6)] == "1")) {
           result(processingSeven_s4_1346(x7s4, o, kods))
  } else if (all(x7s4$sak_beidz[c(1,3,6,7)] == "1")) { 
            result(processingSeven_s4_1367(x7s4, o, kods))
  } else if (all(x7s4$sak_beidz[c(2,3,5,7)] == "1")) {
            result(processingSeven_s4_2357(x7s4, o, kods))
  } else if (all(x7s4$sak_beidz[c(1,3,5,7)] == "1")) {
            result(processingSeven_s4_1212121(x7s4, o, kods))
  } else if (all(x7s4$sak_beidz[c(1,2,4,7)] == "1")) {
            result(processingSeven_s4_1247(x7s4, o, kods))
  } else if (all(x7s4$sak_beidz[c(1,3,5,6)] == "1")) {
            result(processingSeven_s4_1356(x7s4, o, kods))
  } else if (all(x7s4$sak_beidz[c(2,4,6,7)] == "1")) {
            result(processingSeven_s4_2467(x7s4, o, kods))
  } else if (all(x7s4$sak_beidz[c(2,3,6,7)] == "1")) {
           result(processingSeven_s4_2367(x7s4, o, kods))
  } else if (all(x7s4$sak_beidz[c(1,2,4,6)] == "1")) {
          result(processingSeven_s4_1246(x7s4, o, kods))
  } else if (all(x7s4$sak_beidz[c(1,4,6,7)] == "1")) {
          result(processingSeven_s4_1467(x7s4, o, kods))
  } else if (all(x7s4$sak_beidz[c(1,2,5,7)] == "1")) {
          result(processingSeven_s4_1257(x7s4, o, kods))
  } else if (all(x7s4$sak_beidz[c(1,3,4,7)] == "1")) {
          result(processingSeven_s4_1347(x7s4, o, kods))
  } else if (all(x7s4$sak_beidz[c(1,2,5,6)] == "1")) {
          result(processingSeven_s4_1256(x7s4, o, kods))
  } else if (all(x7s4$sak_beidz[c(1,4,5,6)] == "1")) {
          result(processingSeven_s4_1456(x7s4, o, kods))
  } else if (all(x7s4$sak_beidz[c(1,4,5,7)] == "1")) {
          result(processingSeven_s4_1457(x7s4, o, kods))
  } else if (all(x7s4$sak_beidz[c(2,4,5,7)] == "1")) {
          result(processingSeven_s4_2457(x7s4, o, kods))
  } else if (all(x7s4$sak_beidz[c(2,3,5,6)] == "1")) {
          result(processingSeven_s4_2356(x7s4, o, kods))
  } else if (all(x7s4$sak_beidz[c(2,4,5,6)] == "1")) {
          result(processingSeven_s4_2456(x7s4, o, kods))
  } else if (all(x7s4$sak_beidz[c(2,3,4,6)] == "1")) {
          result(processingSeven_s4_2346(x7s4, o, kods))
  } else if (all(x7s4$sak_beidz[c(2,5,6,7)] == "1")) {
          result(processingSeven_s4_2567(x7s4, o, kods))
  } else {stop("processingSeven_s4 trūkst izstrādes koda.\n")}
  
  rm(x7s4, o, kods)
  return(list(x7s4_uzVieniniekiem = x7s4_uzVieniniekiem,
              x7s4_uzDivniekiem   = x7s4_uzDivniekiem,
              x7s4_uzTrijniekiem  = x7s4_uzTrijniekiem,
              x7s4_uzCetriniekiem = x7s4_uzCetriniekiem, 
              x7s4_uzPieciniekiem = x7s4_uzPieciniekiem,
              x7s4_uzSesiniekiem  = x7s4_uzSesiniekiem))
}

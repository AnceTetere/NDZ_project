processingEleven <- function(x, o) {
    x11_uzVieniniekiem <- data.frame()
    x11_uzDesmitniekiem <- data.frame()
    
    for (r in seq(1, nrow(x), by = 11)) {
      x11 <- x[x$PS_code == x$PS_code[r],]
      x11 <- x11[order(x11$PS_code, x11$DN_code, x11$NM_code, x11$NDZ_sanemsanas_datums), ]
      
      if (sum(x11$start == "1") == 6) {
        if ((x11$start[1] == "1" && x11$end[2] == "2") ||(x11$end[1] == "2" && x11$start[2] == "1" && all(diff(x11$NDZ_sanemsanas_datums[1:2]) == 0))) {
          x11_uzVieniniekiem <- rbind(x11_uzVieniniekiem, x11[11,])
          x11_uzDesmitniekiem <- rbind(x11_uzDesmitniekiem, x11[1:10,])
        } else {
          stop("processingEleven: Vienpadsmitnieku tabulas pārdalei trūkst izstrādes koda. Rindas: ", r, " līdz ", r + 10, "\n")
        }
      } else if (sum(x11$end == "2") == 6) {
        if (x11$end[1] == "2" && x11$start[2] == "1" && all(diff(x11$NDZ_sanemsanas_datums[1:2]) != 0)) {
          x11_uzVieniniekiem <- rbind(x11_uzVieniniekiem, x11[1,])
          x11_uzDesmitniekiem <- rbind(x11_uzDesmitniekiem, x11[2:11,])
        } else {
          stop("processingEleven: Vienpadsmitnieku tabulas pārdalei trūkst izstrādes koda. Rindas: ", r, " līdz ", r + 10, "\n")
        }
      } else {
        stop("processingEleven: Vienpadsmitnieku tabulas pārdalei trūkst izstrādes koda. Rindas: ", r, " līdz ", r + 10, "\n")
      }
    }
    
    #2 PĀRBAUDE: Vai rindu skaits no vienpadsmitniekiem atvasinātajās tabulās sakrīt ar rindām izejas tabulā x.
    if ((nrow(x11_uzVieniniekiem) + nrow(x11_uzDesmitniekiem)) == nrow(x)) {
      cat(
        "PĀRBAUDE IZIETA: Apakštabulu rindu summa sakrīt ar izejošo vienpadsmitnieku tabulu."
      )
      rm(x, x11, r)
    } else {
      stop(
        cat(
          "PĀRBAUDE NAV IZIETA: Apakštabulu rindu summa NESAKRĪT ar izejošo vienpadsmitnieku tabulu."
        )
      )
    }
    
    #3 Apakštabulu x11_uzVieniniekiem sūta caur processingOnes().
    if (nrow(x11_uzVieniniekiem) > 0) {
      x11_uzVieniniekiem <-
        x11_uzVieniniekiem[order(
          x11_uzVieniniekiem$PS_code,
          x11_uzVieniniekiem$NM_code,
          x11_uzVieniniekiem$NDZ_sanemsanas_datums
        ),]
      cat(
        "No vienpadsmitniekiem atvasinātā tabula x11_uzVieniniekiem pārsūtīta uz processingOnes un tad uz tempNDZ, ko būvējam."
      )
      sendTo_tempNDZ(processingOnes(x11_uzVieniniekiem, o))
    } else {
      cat("Tabula x11_uzVieniniekiem ir tukša.")
    }
    rm(x11_uzVieniniekiem)
    
    #4 Apakštabulu x11_uzDesmitniekiem sūta caur processingTens().
    if (nrow(x11_uzDesmitniekiem) > 0) {
      x11_uzDesmitniekiem <-
        x11_uzDesmitniekiem[order(
          x11_uzDesmitniekiem$PS_code,
          x11_uzDesmitniekiem$NM_code,
          x11_uzDesmitniekiem$NDZ_sanemsanas_datums
        ),]
      cat(
        "No vienpadsmitniekiem atvasinātā tabula x11_uzDesmitniekiem pārsūtīta uz processingTens un tad uz tempNDZ, ko būvējam."
      )
      processingTens(x11_uzDesmitniekiem, o)
    } else {
      cat("Tabula x11_uzDesmitniekiem ir tukša.")
    }
    rm(x11_uzDesmitniekiem)
}

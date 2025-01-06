tripletkodi3 <- function(y3, t, prev, v) {
  
  yt <- y3[v, ]
  
  if (t$zk[1] %in% c("41", "51", "54", "92")) {
    if (t$zk[2] %in% c("40", "50", "53", "91")) {
      if (t$zk[3] %in% c("21", "22", "23", "24", "25", "29")) {
        if (diff(t$NDZ_sanemsanas_datums[1:2]) == 0 && diff(t$NDZ_sanemsanas_datums[2:3]) != 0) {
          yt$dd <- 1
        } else if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
          yt$dd <- as.numeric(diff(t$NDZ_sanemsanas_datums[1:2]))
        } else {stop("Tripletkodi3 iztrūkst apstrādes koda.")}
      } else {stop("Tripletkodi3 iztrūkst apstrādes koda.")}
    } else if (t$zk[2] %in% c("21", "22", "23", "24", "25", "29")) {
      if (t$zk[3] %in% c("40", "50", "53", "91")) {
        if (diff(t$NDZ_sanemsanas_datums[1:2]) != 0 && diff(t$NDZ_sanemsanas_datums[2:3]) == 0) {
          yt$dd <- as.numeric(diff(t$NDZ_sanemsanas_datums[1:2]))
        } else {stop("Tripletkodi3 iztrūkst apstrādes koda.")}
      } else {stop("Tripletkodi3 iztrūkst apstrādes koda.")}
    } else {stop("Tripletkodi3 iztrūkst apstrādes koda.")}
    # } else if (t$zk[1] %in% c("21", "22", "23", "24", "25", "29")) {
    #           if (t$zk[2] %in% c("41", "51", "54", "92")) {
    #             if (t$zk[3] %in% c("40", "50", "53", "91")) {
    #               if (all(diff(t$NDZ_sanemsanas_datums) == 0)) {
    #                 yt$dd <- 0
    #               } else {stop("Tripletkodi3 iztrūkst apstrādes koda.")}
    #             } else if (t$zk[3] %in% c("41", "51", "54", "92")) {
    #               if (all(diff(t$NDZ_sanemsanas_datums) == 0)) {
    #                 yt$dd <- 0
    #               } else {stop("Tripletkodi3 iztrūkst apstrādes koda.")}
    #             } else {stop("Tripletkodi3 iztrūkst apstrādes koda.")}
    #           } else {stop("Tripletkodi3 iztrūkst apstrādes koda.")}
     } else if (t$zk[1] %in% c("11", "14", "16", "61")) {
             if (t$zk[2] %in% c("40", "50", "53", "91")) {
                 if (t$zk[3] %in% c("40", "50", "53", "91")) {
                  if (all(diff(t$NDZ_sanemsanas_datums) == 0)) {
                    yt$dd <- as.numeric(diff(t$NDZ_sanemsanas_datums[c(1,3)]))
                  } else {stop("Tripletkodi3 iztrūkst apstrādes koda.")}
                } else {stop("Tripletkodi3 iztrūkst apstrādes koda.")}
            } else if (t$zk[2] %in% c("41", "51", "54", "92")) {
                if (t$zk[3] %in% c("40", "50", "53", "91")) {
                  if (diff(t$NDZ_sanemsanas_datums[2:3]) == 0 && diff(t$NDZ_sanemsanas_datums[1:2]) != 0) {
                    if (t$PS_code[1] == '_______________' && t$NM_code[1] == '_______________') {
                      yt$dd <- as.numeric(diff(t$NDZ_sanemsanas_datums[c(1,3)]))
                    } else {stop("Tripletkodi3 iztrūkst apstrādes koda.")}
    #              } else if (all(diff(t$NDZ_sanemsanas_datums) != 0)) {
    #                if (t$PS_code[1] == '_______________' && t$NM_code[1] == '_______________') {
    #                  yt$dd <- as.numeric(diff(t$NDZ_sanemsanas_datums[c(1,3)]))
    #                } else {stop("Tripletkodi3 iztrūkst apstrādes koda.")}
    #              } else {stop("Tripletkodi3 iztrūkst apstrādes koda.")}
    #            } else {stop("Tripletkodi3 iztrūkst apstrādes koda.")}
    #          } else {stop("Tripletkodi3 iztrūkst apstrādes koda.")}
    # } else if (t$zk[1] %in% c("40", "50", "53", "91")) {
  #         if (t$zk[2] %in% c("40", "50", "53", "91")) {
  #           if (t$zk[3] %in% c("21", "22", "23", "24", "25", "29")) {
  #             if (diff(t$NDZ_sanemsanas_datums[2:3]) != 0 && diff(t$NDZ_sanemsanas_datums[1:2]) == 0) {
  #               yt$dd <- as.numeric(difftime(t$NDZ_sanemsanas_datums[1], prev, units = "days")) - 1 
  #             } else {stop("Tripletkodi3 iztrūkst apstrādes koda.")}
  #           } else {stop("Tripletkodi3 iztrūkst apstrādes koda.")}
  #         } else {stop("Tripletkodi3 iztrūkst apstrādes koda.")}
  
                  } else {stop("Tripletkodi3 iztrūkst apstrādes koda.")}
                } else {stop("Tripletkodi3 iztrūkst apstrādes koda.")}
            } else {stop("Tripletkodi3 iztrūkst apstrādes koda.")}
        } else {stop("Tripletkodi3 iztrūkst apstrādes koda.")}


  rm(y3, t, prev, v)
  if(is.na(yt$PS_code[1])) {stop("DD NA.")}
  yt$zk <- "combined"  
  return(yt)
}

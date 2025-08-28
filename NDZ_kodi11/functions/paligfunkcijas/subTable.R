subTable <- function(y_2plus, r, v) {  
   t <- data.frame()

   for(a in 1:r) {
     tName <- paste0("NDZ", year, MM, "_", y_2plus$zk[v + (a-1)])
     load(paste0("data/starptabulas/", year, "/starting_", tName, ".RData"))
     st <- get(tName)
     st <- st %>% dplyr::filter(PS_code == y_2plus$PS_code[v + (a-1)] & NM_code == y_2plus$NM_code[v + (a-1)])
     rm(list = tName, tName)
  
     t <- t %>% rbind(st) %>% arrange(NDZ_sanemsanas_datums)
     rownames(t) <- NULL
     rm(st)
   }

  rm(a, r, v, y_2plus)
  return(t)
}

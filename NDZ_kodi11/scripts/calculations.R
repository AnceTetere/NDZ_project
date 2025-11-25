#kods <- kodu_vektors[3] #FOR TESTING

create_tempZERO()
iztrukstKodi <- character(0)

for(kods in kodu_vektors) {
  #cat("______________________ KODS", kods, "______________________\n")
  NDZ <- NDZ_original(kods)

  if (nrow(NDZ) > 0) {
    cat(processingCodes(NDZ, kods))
    cat(month_byCode(kods))
  } else {
    cat(paste0(year, ". gada ", month, ". mēnesī nav neviena ieraksta  ziņojuma kodam ", kods, "."), "\n")
    iztrukstKodi <- append(iztrukstKodi, kods)
  } 
}

kodu_vektors <- kodu_vektors[!(kodu_vektors %in% iztrukstKodi)]
rm(iztrukstKodi, NDZ, kods)
source("R/scripts/4_months.R")
source("R/scripts/5_output.R")


rm(list = mtab_nos, mtab_nos, kodu_vektors, kods, month, year, md, x)
rm(list = ls())


########################################################################
## Title: Function to perform the full imputation
## Date: 2013-05-27
########################################################################

fullImputation = function(Data, area, prod, yield, country, group, year,
  commodity, n.iter = 1000, tol = 1e-8){
  dataCopy = data.table(Data)
  setnames(dataCopy, old = c(area, prod, yield, country, group, year, commodity),
           new = c("area", "prod", "yield", "country", "group", "year",
             "commodity"))
  setkeyv(dataCopy, c("area", "year"))
  impYield.dt = lmeEMImpute(Data = dataCopy, value = "yield",
    country = "country", group = "group", year = "year", commodity = "commodity",
    n.iter = n.iter, tol = tol)

  setnames(impYield.dt, old = "estValue", new = "imputedYield")
  impYield.dt[!is.na(yield), imputedYield := as.numeric(NA)]
  impYield.dt[, imputedArea := area]
  impYield.dt[, imputedProd := prod]
  impYield.dt[is.na(imputedArea) & !is.na(imputedProd) & !is.na(imputedYield),
           imputedArea := imputedProd/imputedYield]
  impYield.dt[is.na(imputedProd) & !is.na(imputedArea) & !is.na(imputedYield),
           imputedProd := imputedArea * imputedYield]
  
  impYield.dt[, imputedArea := na.locf(na.locf(na.approx2(imputedArea),
                           na.rm = FALSE), fromLast = TRUE, na.rm = FALSE),
              by = c("country", "commodity")]
  impYield.dt[is.na(imputedProd), imputedProd := imputedArea * imputedYield]
  setnames(impYield.dt,
           new = c(area, prod, yield, country, group, year, commodity),
           old = c("area", "prod", "yield", "country", "group", "year",
             "commodity"))  
  impYield.dt
}

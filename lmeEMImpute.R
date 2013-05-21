########################################################################
## Title: Function to impute data with LME and EM estimation of average
## Date: 2013-05-06
########################################################################

lmeEMImpute = function(Data, value, country, group, year, commodity,
  n.iter = 1000, tol = 1e-6){
  setnames(Data, old = c(value, country, group, year, commodity),
           new = c("value", "country", "group", "year", "commodity"))
  for(i in unique(Data$commodity)){
    print(i)
    ll = double(n.iter)
    ll[1] = -Inf
    missInd = is.na(Data[, value])    
    Data[commodity == i, estValue := value]
    ## data[commodity == i, estValue := na.locf(na.locf(na.approx2(estValue,
    ##                        na.rm = FALSE), na.rm = FALSE), fromLast = TRUE),
    ##      by = "country"]
    ## Data[is.na(estValue) & commodity == i,
    ##      estValue := .SD[, Data[, mean(estValue, na.rm = TRUE)]], by = "year"]
    Data[commodity == i, estValue := randomImp(estValue), by = "country"]
    
    for(j in 2:n.iter){
      Data[commodity == i, avgValue := mean(estValue, na.rm = TRUE),
           by = c("year", "group")]
      fit = try(
        lme(value ~ year * group, random= ~avgValue|country,
            na.action = na.omit, data = Data[commodity == i, ])
            )
        ## fit = try(lmer(value ~ year * group + (1 + avgValue + year|country),
        ##   na.action = na.omit, data = Data[commodity == i, ]))
      fit.ll = try(logLik(fit))
      try(print(fit.ll))
      if(!inherits(fit, "try-error")){
        if(fit.ll - ll[j - 1] > tol){
          Data[commodity == i, estValue := predict(fit, Data[commodity == i, ])]
          ## Data[commodity == i & !missInd,
          ##      fittedValue := predict(fit, Data[commodity == i & !missInd, ])]
          Data[commodity == i & !missInd, fittedValue := fitted(fit)]
          ll[j] = fit.ll
        } else {
          break
        }
      } else {
        break
      }
    }
  }
  setnames(Data, old = c("value", "country", "group", "year", "commodity"),
           new = c(value, country, group, year, commodity))
}

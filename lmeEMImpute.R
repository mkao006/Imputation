########################################################################
## Title: Function to impute data with LME and EM estimation of average
## Date: 2013-05-06
########################################################################

lmeEMImpute = function(Data, value, country, group, year, commodity,
  n.iter = 1000, tol = 1e-6, silent = TRUE){
  setnames(Data, old = c(value, country, group, year, commodity),
           new = c("value", "country", "group", "year", "commodity"))
  setkeyv(Data, c("country", "commodity", "group", "year"))
  for(i in unique(Data$commodity)){
    print(i)
    ## Initialization
    ll = rep(NA, n.iter + 1)
    ll[1] = -Inf
    missInd = is.na(Data[, value])    
    Data[commodity == i, estValue := value]
    method = "naive"
    
    ## Naive imputation
    Data[commodity == i, estValue := naiveImp(estValue), by = "country"]
    
    ## EM-lme
    for(j in 1:n.iter){
      Data[commodity == i, avgValue := mean(estValue, na.rm = TRUE),
           by = c("year", "group")]
      fit = try(
        lme(value ~ year * group, random = ~avgValue|country,
            na.action = na.omit, data = Data[commodity == i, ])
            )
      fit.ll = try(logLik(fit))
      if(!silent)
        try(print(fit.ll))
      if(!inherits(fit, "try-error")){
        if(fit.ll - ll[j] > tol){
          Data[commodity == i,
                   estValue := predict(fit, Data[commodity == i, ])]
          ll[j + 1] = fit.ll
          method = "LME"
        } else {
          break
        }
      } else {
        break
      }
    }
  }
  ## Outputs
  setnames(Data, old = c("value", "country", "group", "year", "commodity"),
           new = c(value, country, group, year, commodity))
  list(imputed = Data, method = method)
}

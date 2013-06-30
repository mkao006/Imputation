########################################################################
## Title: Function to impute data with LME and EM estimation of average
## Date: 2013-05-06
########################################################################

lmeImpute = function(fixed = value ~ 0, random = ~1|country,
  group = ~ year * region, Data, n.iter = 1000, tol = 1e-6, silent = TRUE){

  old.class = class(Data)
  Data = data.table(Data)

  ## Initialization
  imputeVar = as.character(fixed[[2]])
  condVar = as.character(random[[2]][[3]])
  groupVar = all.vars(group[[2]])
  setkeyv(Data, cols = c(condVar, groupVar))
  fixed = update(old = fixed, new = group)
  null.ll = -Inf
  naive.ll = -Inf
  lme.ll = rep(NA, n.iter + 1)
  lme.ll[1] = -Inf  
  missInd = is.na(Data[, eval(parse(text = imputeVar))])

  ## Naive imputation
  Data[, eval(parse(text = paste0("naiveImp := naiveImp(",
                      imputeVar, ")"))), by = condVar]
  
  ## Null model without grouped average effects
  null.fit = try(
    do.call("lme",
            list(fixed = fixed, random = random, na.action = na.omit, data = Data,
                 method = "ML")), silent = TRUE
    )

  if(is.finite(logLik(null.fit))){
    null.ll = logLik(null.fit)
    Data[, lmeImp := predict(null.fit, Data)]
  }
  
  ## lme with grouped average effects
  for(j in 1:n.iter){
    if(j == n.iter)
      print("maximum iteration reached, model may have not converged")

    Data[, lmeMeanImp := naiveImp]
    Data[, groupAverage := mean(lmeMeanImp, na.rm = TRUE), by = groupVar]

    groupedFixed = update(fixed, ~. + groupAverage)

    fit = try(
      do.call("lme",
              list(fixed = groupedFixed, random = random, na.action = na.omit,
                   data = Data, method = "ML")), silent = TRUE
      )

    if(is.finite(logLik(fit))){
      fit.ll = logLik(fit)
      if(fit.ll - lme.ll[j] > tol){
        Data[!is.na(groupAverage),
             lmeMeanImp := predict(fit, Data[!is.na(groupAverage), ])]
        lme.ll[j + 1] = fit.ll
      } else {
        break
      }
    } else {
      break
    }
  }

  ## Select imputation method
  bestImp = c("naiveImp", "lmeImp", "lmeMeanImp")[
    which.max(c(naive.ll, null.ll, fit.ll))]
  
  Data[, eval(parse(text = paste0("impValue := ", bestImp)))]
  
  
  class(Data) = old.class
  list(imputed = Data, method = bestImp, model = fit)
}
